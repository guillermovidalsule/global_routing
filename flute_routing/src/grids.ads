with Ada.Containers.Ordered_Sets;

package Grids is

   package Integer_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Integer);

   type Edge_Record is
     record
      Capacity : Integer;
      Net_Ids : Integer_Sets.Set;
     end record;

   type Edge_Array is array (Integer range <>, Integer range <>)
     of Edge_Record;

   type Grid_Record (Low_H, High_H, Low_V, High_V : Integer) is
     record
        Vertical_Edges, Horizontal_Edges :
          Edge_Array (Low_H .. High_H, Low_V .. High_V);
     end record;

   function Add_Net_To_Edge
     (Edge : in out Edge_Record; New_Id : Integer)
     return Boolean;

   procedure Remove_Net_From_Edge
     (Edge : in out Edge_Record; Id : Integer);

end Grids;
