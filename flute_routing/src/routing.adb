with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

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
   begin
      for I in Net_List.First_Index .. Net_List.Last_Index loop
         if not Route_Net (Grid, Net_List (I).all, I) then
            return False;
         end if;
      end loop;
      return True;
   end Route;

   -----------------
   --  Route_Net  --
   -----------------

   function Route_Net (Grid : in out Grids.Grid_Record;
                       Net : Nets.Net_Record;
                       Net_Id : Integer)
   return Boolean is

      --  Loads the FLUTE precalc data from the .dat files
      procedure Read_LUT
        with Convention => C,
             External_Name => "readLUT",
             Import => True;

      --  Performs a blind FLUTE
      function Flute (Degree : Interfaces.C.int;
                      X, Y : Nets.Coordinate_Array;
                      Accuracy : Interfaces.C.int)
        return Tree
        with Convention => C,
             External_Name => "flute",
             Import => True;

      Result : Tree;
      Routed : Boolean := True;

   begin
      --  Initialize FLUTE LUT
      Read_LUT;
      --  Run blind FLUTE
      Result := Flute (Interfaces.C.int (Net.Degree),
        Net.X, Net.Y, Accuracy);

      --  Route each branch from the tree
      declare
         Branches : Branch_Array
           (1 .. 2 * Integer (Result.Degree) - 2)
           with Address => Result.Branch;
         Target, Current : Point;

         function To_Point (Branch : Branch_Record) return Point is
           (Integer (Branch.X), Integer (Branch.Y));

      begin
         for I in Branches'Range loop
            Current := To_Point (Branches (I));
            Target := To_Point (Branches (Integer (Branches (I).Neighbor) + 1));
            --  Is a steiner point by FLUTE ilegal?
            if Is_Obstacle (Grid, Target) then
               Target := Find_Nearest (Grid, @);
               Branches (Integer (Branches (I).Neighbor) + 1).X :=
                 Interfaces.C.int (Target.X);
               Branches (Integer (Branches (I).Neighbor) + 1).Y :=
                 Interfaces.C.int (Target.Y);
            end if;

            if Is_Obstacle (Grid, Current) then
               Current := Find_Nearest (Grid, @);
               Branches (I).X := Interfaces.C.int (Current.X);
               Branches (I).Y := Interfaces.C.int (Current.Y);
            end if;

            --  Avoid repetitive branches and root node
            if Integer (Branches (I).X) /= Target.X or
               Integer (Branches (I).Y) /= Target.Y
            then
               Routed := Route_Points (Grid, Integer (Branches (I).X),
                 Integer (Branches (I).Y), Target.X, Target.Y, Net_Id) and @;
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
