pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Input_File     : File_Type;
   Previous_Depth : Integer := Integer'First;
   Increased      : Integer := 0;
   Skip_First     : Boolean := True;

   -- part 2
   Line_Counter : Integer := 0;
   type Three_Item_Array is array (1 .. 3) of Integer;


begin
   -- Part 1
   Open (Input_File, In_File, "input.txt");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Current_Depth : Integer :=
           Integer'Value (Get_Line (File => Input_File));
      begin
         Line_Counter := Line_Counter + 1;
         if not Skip_First then
            if Current_Depth > Previous_Depth then
               Increased := Increased + 1;
            end if;
            Previous_Depth := Current_Depth;
         else
            Skip_First := False;
         end if;
      end;
   end loop;
   Close (File => Input_File);
   Put_Line ("Increased " & Increased'Image);

   -- Part 2

   declare
      type Depth_Array_Type is array (1 .. Line_Counter) of Integer;
      Depth_Arr : Depth_Array_Type;
      Index : Positive := Depth_Arr'First;
      Increased_Sum_Counter : Natural := 0;
      Last_Sum: Integer := 0;
   begin
      Line_Counter := 1;
      Open (Input_File, In_File, "input.txt");
      loop
         exit when End_Of_File (File => Input_File);
         declare
            Current_Depth : Integer :=
              Integer'Value (Get_Line (File => Input_File));
         begin
            Depth_Arr (Line_Counter) := Current_Depth;
            Line_Counter := Line_Counter + 1;
         end;
      end loop;
      Close (File => Input_File);

      loop
         declare
            Current_Sum: Integer;
         begin
            exit when Index + 2 > Depth_Arr'Last;

            Current_Sum := Depth_Arr(Index) + Depth_Arr(Index +1) + Depth_Arr(Index + 2);
            if Current_Sum > Last_Sum and Last_Sum /= 0 then
               Increased_Sum_Counter := Increased_Sum_Counter + 1;
            end if;
            Last_Sum := Current_Sum;
            Index := Index + 1;
         end;
      end loop;
      Put_Line ("Increased " & Increased_Sum_Counter'Image);
   end;



end Main;
