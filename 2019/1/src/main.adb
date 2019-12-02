with Ada.Text_IO; use Ada.Text_IO;
procedure Main is

   function Compute_Fuel (Mass : Float) return Integer is
   begin
      return Integer (Float'Floor (Mass / 3.0) - 2.0);
   end Compute_Fuel;

   Input_File      : File_Type;
   Sum_Of_Fuel     : Natural := 0;
   Additional_Fuel : Natural := 0;

begin
   Open (Input_File, In_File, "input.txt");
   loop
      declare
         Line                : String  := Get_Line (File => Input_File);
         Module_Mass         : Float   := Float'Value (Line);
         Current_Module_Fuel : Integer := Compute_Fuel (Module_Mass);
      begin
         -- part 1
         Sum_Of_Fuel := Sum_Of_Fuel + Current_Module_Fuel;
         -- part 2 get fuel needed to lift fuel

         loop
            Current_Module_Fuel := Compute_Fuel (Float(Current_Module_Fuel));
            exit when Current_Module_Fuel <= 0;

            Additional_Fuel     := Additional_Fuel + Current_Module_Fuel;
         end loop;
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (File => Input_File);
   Put_Line ("Part 1 " & Sum_Of_Fuel'Image);
   declare
      Total_Fuel : Integer := Sum_Of_Fuel + Additional_Fuel;
   begin
      Put_Line ("Part 2" & Total_Fuel'Img);
   end;
end Main;
