with Interfaces.C, System;
with Grids, Nets;

package Routing is

   Min_Capacity : constant := $CAP;
   Accuracy : constant := $ACC;

   type Branch_Record is
     record
      X, Y, Neighbor : Interfaces.C.int;
     end record
     with Convention => C;

   type Branch_Array is array (Integer range <>) of aliased Branch_Record
     with Convention => C;

   type Tree is
     record
      Degree : Interfaces.C.int;
      Length : Interfaces.C.int;
      Branch : System.Address;
     end record
     with Convention => C;

   function Route (Grid : in out Grids.Grid_Record;
                   Net_List : Nets.Net_Vectors.Vector)
     return Boolean;

   function Route_Net (Grid : in out Grids.Grid_Record;
                       Net : Nets.Net_Record;
                       Net_Id : Integer)
     return Boolean;

   function Route_Points (Grid : in out Grids.Grid_Record;
                          X_1, Y_1, X_2, Y_2, Net_Id : Integer)
     return Boolean;

end Routing;
