with Ada.Text_IO;

procedure Main is
   use Ada;

   type Char_Counter_Arr is array (Character range 'a' .. 'z') of Natural;

   Input_File    : Text_IO.File_Type;
   Two_Letters   : Natural := 0;
   Three_Letters : Natural := 0;
begin
   Text_IO.Open
     (File => Input_File,
      Mode => Text_IO.In_File,
      Name => "input.txt");
   loop
      exit when Text_IO.End_Of_File (File => Input_File);
      declare
         Line         : String := Text_IO.Get_Line (File => Input_File);
         Char_Counter : Char_Counter_Arr := (others => 0);
      begin
         for I in Line'Range loop
            Char_Counter (Line (I)) := Char_Counter (Line (I)) + 1;
         end loop;

         for I in Char_Counter'Range loop
            if Char_Counter (I) = 2 then
               Two_Letters := Two_Letters + 1;
               exit;
            end if;
         end loop;

         for I in Char_Counter'Range loop
            if Char_Counter (I) = 3 then
               Three_Letters := Three_Letters + 1;
               exit;
            end if;
         end loop;

      end;
   end loop;
   Text_IO.Put_Line
     (Natural'Image (Two_Letters) &
        " " &
        Natural'Image (Three_Letters) &
        " = " &
        Natural'Image (Two_Letters * Three_Letters));
end Main;
