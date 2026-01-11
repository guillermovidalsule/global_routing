with Ada.Command_Line, Ada.Text_IO;
with Routing;
with Grids;
with Nets;

procedure Flute_Routing is

   Result : Boolean;

   procedure Export_Routing (Grid : Grids.Grid_Record; File_Name : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
      Ada.Text_IO.Put_Line (File, "Type,X1,Y1,X2,Y2,Nets");

      for X in Grid.Vertical_Edges'Range (1) loop
         for Y in Grid.Vertical_Edges'Range (2) loop
            if not Grid.Vertical_Edges (X, Y).Net_Ids.Is_Empty then
               Ada.Text_IO.Put (File, "V," & X'Image & "," & Y'Image &
                 "," & Integer (X + 1)'Image & "," & Y'Image & ",");
               for Id of Grid.Vertical_Edges (X, Y).Net_Ids loop
                  Ada.Text_IO.Put (File, Id'Image & " ");
               end loop;
               Ada.Text_IO.New_Line (File);
            end if;
         end loop;
      end loop;

      for X in Grid.Horizontal_Edges'Range (1) loop
         for Y in Grid.Horizontal_Edges'Range (2) loop
            if not Grid.Horizontal_Edges (X, Y).Net_Ids.Is_Empty then
               Ada.Text_IO.Put (File, "H," & X'Image & "," & Y'Image &
                 "," & X'Image & "," & Integer (Y + 1)'Image & ",");
               for Id of Grid.Horizontal_Edges (X, Y).Net_Ids loop
                  Ada.Text_IO.Put (File, Id'Image & " ");
               end loop;
               Ada.Text_IO.New_Line (File);
            end if;
         end loop;
      end loop;

      Ada.Text_IO.Close (File);
   end Export_Routing;

begin

   --  Read command line arguments
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Wrong number of arguments!");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Usage: ./bin NET_FILEPATH EDGE_FILEPATH");
      return;
   end if;

   declare
      Net_File : String := Ada.Command_Line.Argument (1);
      Edge_File : String := Ada.Command_Line.Argument (2);
      Start_H, H_Size, Start_V, V_Size : Integer;

      package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
      F_Nets, F_Edges : Ada.Text_IO.File_Type;
   begin
      Net_File := Ada.Command_Line.Argument (1);
      Edge_File := Ada.Command_Line.Argument (2);

      --  Read size of the Grid
      Ada.Text_IO.Open (F_Edges, Ada.Text_IO.In_File, Edge_File);
      Integer_IO.Get (F_Edges, H_Size);
      Integer_IO.Get (F_Edges, V_Size);
      Integer_IO.Get (F_Edges, Start_H);
      Integer_IO.Get (F_Edges, Start_V);

      declare
         Grid : aliased Grids.Grid_Record
          (Start_H, Start_H + H_Size - 1, Start_V, Start_V + V_Size - 1);
         Net_List : Nets.Net_Vectors.Vector;
         Net_Degree : Integer;
      begin

         --  Set up the edges capacities
         for E of Grid.Horizontal_Edges loop
            Integer_IO.Get (F_Edges, E.Capacity);
            E.Capacity := (if @ = 0 then Integer'First else @);
         end loop;
         for E of Grid.Vertical_Edges loop
            Integer_IO.Get (F_Edges, E.Capacity);
            E.Capacity := (if @ = 0 then Integer'First else @);
         end loop;

         --  Set up the nets
         Ada.Text_IO.Open (F_Nets, Ada.Text_IO.In_File, Net_File);
         while not Ada.Text_IO.End_Of_File (F_Nets) loop
            Integer_IO.Get (F_Nets, Net_Degree);
            declare
               New_Net : Nets.Net_Ptr := new Nets.Net_Record (Net_Degree);
            begin
               for I in 1 .. Net_Degree loop
                  Integer_IO.Get (F_Nets, New_Net.X (I));
                  Integer_IO.Get (F_Nets, New_Net.Y (I));
                  if New_Net.X (I) not in Grid.Vertical_Edges'Range (1) or
                     New_Net.Y (I) not in Grid.Horizontal_Edges'Range (2)
                  then
                     Ada.Text_IO.Put_Line ("Point X =" & New_Net.X (I)'Image
                       & " Y =" & New_Net.Y (I) 'Image &
                       " does not belong to grid!");
                     return;
                  end if;
               end loop;
               Net_List.Append (New_Net);
            end;
         end loop;

         Result := Routing.Route (Grid, Net_List);
         if not Result then
            Ada.Text_IO.Put_Line ("Routing was not possible");
         else
            Ada.Text_IO.Put_Line ("Exporting data");
            Export_Routing (Grid, "results.csv");
         end if;

      end;
   end;

end Flute_Routing;
