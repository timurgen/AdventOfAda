pragma Ada_2020;

with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Main is
   type Row_Defs is (F, B);
   type Col_Defs is (R, L);
   type Boarding_Pass is new String (1 .. 10);

   function Parse_Pass_Id (Pass : Boarding_Pass) return Natural is
      Row_Low    : Natural := 0;
      Row_High   : Natural := 127;
      Target_Row : Natural;

      Col_Low    : Natural := 0;
      Col_High   : Natural := 7;
      Target_Col : Natural;

      Id : Natural;
   begin
      -- find row
      for I in 1 .. 6 loop
         if Row_Defs'Value (Pass (I) & "") = F then
            Row_High :=
              Natural (Float'Floor (Float (Row_High + Row_Low) / 2.0));
         elsif Row_Defs'Value (Pass (I) & "") = B then
            Row_Low :=
              Natural (Float'Rounding (Float (Row_High + Row_Low) / 2.0));
         end if;
      end loop;
      Target_Row :=
        (if Row_Defs'Value (Pass (7) & "") = B then Row_High else Row_Low);

      -- find column
      for I in 8 .. 9 loop
         if Col_Defs'Value (Pass (I) & "") = L then
            Col_High :=
              Natural (Float'Floor (Float (Col_High + Col_Low) / 2.0));
         elsif Col_Defs'Value (Pass (I) & "") = R then
            Col_Low :=
              Natural (Float'Rounding (Float (Col_High + Col_Low) / 2.0));
         end if;
      end loop;
      Target_Col :=
        (if Col_Defs'Value (Pass (10) & "") = R then Col_High else Col_Low);

      Id := Target_Row * 8 + Target_Col;
      -- Put_Line (Pass'Image & Target_Row'Image & Target_Col'Image &
      -- Id'Image);
      return Id;
   end Parse_Pass_Id;

   package Pass_Vector is new Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Natural);
   Pass_List : Pass_Vector.Vector;

   package Pass_Sorter is new Pass_Vector.Generic_Sorting
     ("<" => Standard."<");

   Input_File : File_Type;

   Highiest_Id : Natural := 0;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Current_Pass : Boarding_Pass :=
           Boarding_Pass (Get_Line (File => Input_File));
         Pass_Id : Natural := Parse_Pass_Id (Current_Pass);
      begin
         Pass_List.Append (Pass_Id);
         if Pass_Id > Highiest_Id then
            Highiest_Id := Pass_Id;
         end if;
      end;
      exit when End_Of_File (File => Input_File);
   end loop;
   Close (File => Input_File);
   Put_Line ("part 1" & Highiest_Id'Image);
   -- part 2
   Pass_Sorter.Sort (Pass_List);
   for I in Pass_List.First_Index + 1 .. Pass_List.Last_Index loop
      if Pass_List.Element (I) - Pass_List.Element (I - 1) > 1 then
         declare
            Part2_Result : Integer := Pass_List.Element (I) - 1;
         begin
            Put_Line ("part 2 " & Part2_Result'Image);
         end;
      end if;
   end loop;
end Main;
