with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;


procedure Main is
   function Compare_String(Left, Right: String) return Boolean is (Left < Right);

   package String_V is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
   package String_V_Sorter is new String_V.Generic_Sorting("<" => Compare_String);

   Raw_Input_Data_Vector : String_V.Vector;

   Input_File : File_Type;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      exit when End_Of_File (Input_File);
      declare
         Line : String := Get_Line (Input_File);
      begin
         Raw_Input_Data_Vector.Append (New_Item => Line);
      end;
   end loop;
   Close (File => Input_File);
   String_V_Sorter.Sort(Container => Raw_Input_Data_Vector);
end Main;
