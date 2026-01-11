with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with System;

package body Routing is

   --------------------
   --  Private spec  --
   --------------------

   type Point is
     record
      X, Y : Integer;
     end record;

   type Direction is (Up, Left, Down, Right);

   type Queue_Element is record
      P : Point;
      F_Score : Integer;
   end record;

   package Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Queue_Element);

   function Get_Priority (Element : Queue_Element) return Integer is
      (Element.F_Score);

   function Before (Left, Right : Integer) return Boolean is
      (Left < Right);

   --------------------------
   --  Operator Overloads  --
   --------------------------

   function "<" (Left, Right : Point) return Boolean is
   begin
      if Left.X /= Right.X then
         return Left.X < Right.X;
      else
         return Left.Y < Right.Y;
      end if;
   end "<";

   function "+" (L : Point; R : Direction)
   return Point is
      Result : Point := L;
   begin
      case R is
         when Up    => Result.X := @ - 1;
         when Left  => Result.Y := @ - 1;
         when Down  => Result.X := @ + 1;
         when Right => Result.Y := @ + 1;
      end case;
      return Result;
   end "+";

   function "not" (L : Direction) return Direction is
   begin
      case L is
         when Up => return Down;
         when Down => return Up;
         when Left => return Right;
         when Right => return Left;
      end case;
   end "not";

   --------------------
   --  Has_Capacity  --
   --------------------

   function Has_Capacity (Grid : Grids.Grid_Record;
                          P : Point; D : Direction;
                          Capacity : Integer)
   return Boolean is
   begin
      case D is
         when Right =>
            return Grid.Horizontal_Edges (P.X, P.Y).Capacity > Capacity;
         when Left =>
            return Grid.Horizontal_Edges (P.X, P.Y - 1).Capacity > Capacity;
         when Down =>
            return Grid.Vertical_Edges (P.X, P.Y).Capacity > Capacity;
         when Up =>
            return Grid.Vertical_Edges (P.X - 1, P.Y).Capacity > Capacity;
      end case;
   exception
      when Constraint_Error => return False;
   end Has_Capacity;

   package Priority_Queues is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Queue_Interfaces,
      Queue_Priority  => Integer,
      Get_Priority    => Get_Priority,
      Before          => Before);

   package Point_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Point,
      Element_Type => Integer);

   ----------------
   --  Feasible  --
   ----------------

   --  Is a given point within the Grid?
   function Feasible (Grid : Grids.Grid_Record; P : Point)
     return Boolean is
   begin
      return P.X in Grid.Vertical_Edges'Range (1) and
             P.Y in Grid.Horizontal_Edges'Range (2);
   end Feasible;

   -------------------
   --  Is_Obstacle  --
   -------------------

   function Is_Obstacle (Grid : Grids.Grid_Record; P : Point) return Boolean is
   begin
      --  Tries to find an edge with an unblocked capacity
      for D in Direction loop
         if Feasible (Grid, P + D) then
            if Has_Capacity (Grid, P, D, Integer'First) then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Obstacle;

   ------------------------------------
   --  Calculate_Manhattan_Distance  --
   ------------------------------------

   function Calculate_Manhattan_Distance (P_1, P_2 : Point)
     return Natural is
   begin
      return abs (P_1.X - P_2.X) + abs (P_1.Y - P_2.Y);
   end Calculate_Manhattan_Distance;

   --------------------
   --  Find_Nearest  --
   --------------------

   function Find_Nearest (Grid : Grids.Grid_Record; P : Point)
     return Point
   is
      Search_Queue : Priority_Queues.Queue;
      Visited      : Point_Maps.Map;
      Curr_Item    : Queue_Element;
      Next         : Point;
      use type Ada.Containers.Count_Type;
   begin
      Search_Queue.Enqueue ((P => P, F_Score => 0));
      Visited.Include (P, 0);

      while Search_Queue.Current_Use > 0 loop
         Search_Queue.Dequeue (Curr_Item);

         if not Is_Obstacle (Grid, Curr_Item.P) then
            return Curr_Item.P;
         end if;

         for D in Direction loop
            Next := Curr_Item.P + D;
            if Feasible (Grid, Next) and then
               not Visited.Contains (Next)
            then
               Visited.Include (Next, 0);
               Search_Queue.Enqueue
                 ((P => Next, F_Score => Curr_Item.F_Score + 1));
            end if;
         end loop;
      end loop;

      return P;
   end Find_Nearest;

   -------------
   --  Route  --
   -------------

   function Route (Grid : in out Grids.Grid_Record;
                   Net_List : Nets.Net_Vectors.Vector)
   return Boolean is

      procedure Load_Geosteiner
        with Convention => C,
             External_Name => "gst_open_geosteiner",
             Import => True;

      procedure Close_Geosteiner
        with Convention => C,
             External_Name => "gst_close_geosteiner",
             Import => True;

   begin
      Load_Geosteiner;
      for I in Net_List.First_Index .. Net_List.Last_Index loop
         if not Route_Net (Grid, Net_List (I).all, I) then
            Close_Geosteiner;
            return False;
         end if;
      end loop;
      Close_Geosteiner;
      return True;
   end Route;

   -----------------
   --  Route_Net  --
   -----------------

   function Route_Net (Grid : in out Grids.Grid_Record;
                       Net : Nets.Net_Record;
                       Net_Id : Integer)
     return Boolean is

      Routed : Boolean := True;

      use Interfaces.C;
      type Double_Array is array (size_t range <>) of aliased double;
      type Int_Array    is array (size_t range <>) of aliased int;

      procedure  Get_RSMT (
         nterms  : int;
         terms   : System.Address; -- double*
         length  : System.Address; -- double*
         nsps    : System.Address; -- int*
         sps     : System.Address; -- double*
         nedges  : System.Address; -- int*
         edges   : System.Address; -- int*
         status  : System.Address; -- int*
         param   : System.Address  -- gst_param_ptr
      )
      with Import => True, Convention => C, External_Name => "gst_rsmt";

      N       : constant int := int (Net.Degree);
      Terms   : aliased Double_Array (0 .. size_t (N * 2 - 1));
      
      Length  : aliased double;
      NSPS    : aliased int;
      NEdges  : aliased int;
      Status  : aliased int;
      
      SPS     : aliased Double_Array (0 .. size_t ((N - 2) * 2 - 1)) := (others => 0.0);
      Edges   : aliased Int_Array (0 .. size_t ((2 * N - 3) * 2 - 1)) := (others => 0);

   begin
      for J in 0 .. Net.Degree - 1 loop
         Terms (size_t (J * 2))     := double (Net.X (J));
         Terms (size_t (J * 2 + 1)) := double (Net.Y (J));
      end loop;

      Get_RSMT (
         nterms  => N,
         terms   => Terms (0)'Address,
         length  => Length'Address,
         nsps    => NSPS'Address,
         sps     => SPS (0)'Address,
         nedges  => NEdges'Address,
         edges   => Edges (0)'Address,
         status  => Status'Address,
         param   => System.Null_Address
      );

      if Status /= 0 then
         return False;
      end if;

      declare
         P1, P2 : Point;
         ID_A, ID_B : int;

         function Resolve (ID : int) return Point is
         begin
            if ID < N then
               return (X => Integer (Terms (size_t (ID * 2))),
                       Y => Integer (Terms (size_t (ID * 2 + 1))));
            else
               declare
                  Offset : constant size_t := size_t (ID - N);
               begin
                  return (X => Integer (SPS (Offset * 2)),
                          Y => Integer (SPS (Offset * 2 + 1)));
               end;
            end if;
         end Resolve;

      begin
         for E in 0 .. Integer (NEdges) - 1 loop
            ID_A := Edges (size_t (E * 2));
            ID_B := Edges (size_t (E * 2 + 1));

            P1 := Resolve (ID_A);
            P2 := Resolve (ID_B);

            if Is_Obstacle (Grid, P1) then
               P1 := Find_Nearest (Grid, P1);
            end if;

            if Is_Obstacle (Grid, P2) then
               P2 := Find_Nearest (Grid, P2);
            end if;

            if P1 /= P2 then
               Routed := Route_Points (Grid, P1.X, P1.Y, P2.X, P2.Y, Net_Id) and Routed;
            end if;
         end loop;
      end;

      return Routed;
   end Route_Net;

   --------------------
   --  Route_Points  --
   --------------------

   function Route_Points (Grid : in out Grids.Grid_Record;
                          X_1, Y_1, X_2, Y_2, Net_Id : Integer)
   return Boolean is
      Open_Set : Priority_Queues.Queue;
      G_Scores : Point_Maps.Map;

      Current_Item : Queue_Element;
      Start : constant Point := (X_1, Y_1);
      Goal : constant Point := (X_2, Y_2);

      package Parent_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type     => Point,
         Element_Type => Direction);

      Came_From : Parent_Maps.Map;
      E : Point;

      use Ada.Containers;
   begin
      --  Set up initial state of the structures
      G_Scores.Include ((X_1, Y_1), 0);
      Open_Set.Enqueue ((P => (X_1, Y_1), F_Score =>
        Calculate_Manhattan_Distance ((X_1, Y_1), Goal)));

      while Open_Set.Current_Use > 0 loop
         Open_Set.Dequeue (Current_Item);

         --  Undo the path
         if Current_Item.P = Goal then
            E := Current_Item.P;
            loop
               declare
                  Dir : constant Direction := Came_From (E);
                  Next : constant Point := E + Dir;
                  Is_New_Edge : Boolean := True;
               begin
                  if Dir = Up or Dir = Down then
                     Is_New_Edge :=
                     Grids.Add_Net_To_Edge (Grid.Vertical_Edges
                       (Integer'Min (E.X, Next.X), E.Y), Net_Id);
                  else
                     Is_New_Edge :=
                     Grids.Add_Net_To_Edge (Grid.Horizontal_Edges
                       (E.X, Integer'Min (E.Y, Next.Y)), Net_Id);
                  end if;
                  if not Is_New_Edge then
                     return True;
                  end if;
                  E := Next;
               end;
               exit when E = Start;
            end loop;
            return True;
         end if;

         --  A*
         for D in Direction loop
            declare
               Next : constant Point := Current_Item.P + D;
            begin
               if Feasible (Grid, Next) and then
                  Has_Capacity (Grid, Current_Item.P, D, Min_Capacity)
               then
                  declare
                     New_G : constant Integer := G_Scores (Current_Item.P) + 1;
                  begin
                     if not G_Scores.Contains (Next) or else
                        New_G < G_Scores (Next)
                     then
                        G_Scores.Include (Next, New_G);
                        Came_From.Include (Next, not D);
                        Open_Set.Enqueue ((P => Next, F_Score =>
                          New_G + Calculate_Manhattan_Distance (Next, Goal)));
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;

      return False;
   end Route_Points;

end Routing;
