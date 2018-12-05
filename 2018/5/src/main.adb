with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;
procedure Main is
   Input_File : File_Type;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      exit when End_Of_File (Input_File);
      declare
         Line   : String  := Get_Line (File => Input_File);
         Cursor : Positive := Line'First;
      begin
         loop
            exit when Cursor >= Line'Last;
            if
              (Is_Lower (Line (Cursor)) and then Is_Upper (Line (Cursor + 1))
               and then Line (Cursor) = To_Lower (Line (Cursor + 1)))
              or else
                (Is_Upper (Line (Cursor)) and then Is_Lower (Line (Cursor + 1))
                 and then Line (Cursor) = To_Upper (Line (Cursor + 1)))
            then
               Ada.Strings.Fixed.Delete (Line, Cursor, Cursor + 1);
               if Cursor > 1 then
                  Cursor := Cursor - 1;
               end if;

            else
               Cursor := Cursor + 1;
            end if;
         end loop;
         Put_Line (Line'Length'Img);
      end;
   end loop;
   Close (File => Input_File);
end Main;
