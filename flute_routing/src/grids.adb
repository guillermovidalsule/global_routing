package body Grids is

   function Add_Net_To_Edge
     (Edge : in out Edge_Record; New_Id : Integer)
   return Boolean is
   begin
      if not Edge.Net_Ids.Contains (New_Id) then
         Edge.Capacity := @ - 1;
         Edge.Net_Ids.Include (New_Id);
         return True;
      else
         return False;
      end if;
   end Add_Net_To_Edge;

   procedure Remove_Net_From_Edge
     (Edge : in out Edge_Record; Id : Integer)
   is
   begin
      Edge.Capacity := @ + 1;
      Edge.Net_Ids.Delete (Id);
   end Remove_Net_From_Edge;

end Grids;
