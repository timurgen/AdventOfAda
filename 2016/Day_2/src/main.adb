with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Keypad_Button is range 1 .. 9;

   type Keypad_Array is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of Keypad_Button;

   type Keypad_Button_P2 is new Character range '0' .. 'D';

   type Keypad_Array_P2 is
     array (Positive range 1 .. 5, Positive range 1 .. 5) of Keypad_Button_P2;

   Assert_Exception : exception;

   Current_Button    : Keypad_Button            := 5;
   Current_Button_P2 : Keypad_Button_P2         := '5';
   Current_Row       : Positive                 := 2;
   Current_Column    : Positive                 := 2;
   Keypad : constant Keypad_Array    := ((1, 2, 3), (4, 5, 6), (7, 8, 9));
   Keypad_P2         : constant Keypad_Array_P2 :=
     (('0', '0', '1', '0', '0'), ('0', '2', '3', '4', '0'),
      ('5', '6', '7', '8', '9'), ('0', 'A', 'B', 'C', '0'),
      ('0', '0', 'D', '0', '0'));

   Input_File : File_Type;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt", Form => "");
   Put ("Part 1 code: ");
   loop
      declare
         Line : String := Get_Line (File => Input_File);
      begin
         for C in Line'Range loop
            case Line (C) is
               when 'U' =>
                  if Current_Row > 1 then
                     Current_Row := Current_Row - 1;
                  end if;
               when 'D' =>
                  if Current_Row < 3 then
                     Current_Row := Current_Row + 1;
                  end if;
               when 'L' =>
                  if Current_Column > 1 then
                     Current_Column := Current_Column - 1;
                  end if;
               when 'R' =>
                  if Current_Column < 3 then
                     Current_Column := Current_Column + 1;
                  end if;
               when others =>
                  raise Assert_Exception;
            end case;
            Current_Button := Keypad (Current_Row, Current_Column);
         end loop;
         Put (Current_Button'Image);
      end;
      exit when End_Of_File (Input_File);
   end loop;

   New_Line;
   Reset (File => Input_File);
   Current_Row    := 3;
   Current_Column := 1;
   Put ("Part 2 code: ");

   loop
      declare
         Line : String := Get_Line (File => Input_File);
      begin
         for C in Line'Range loop
            case Line (C) is
               when 'U' =>
                  if Current_Row > 1 and then
                    Keypad_P2 (Current_Row - 1, Current_Column) /= '0' then
                     Current_Row := Current_Row - 1;
                  end if;
               when 'D' =>
                  if Current_Row < 5 and then
                    Keypad_P2 (Current_Row + 1, Current_Column) /= '0' then
                     Current_Row := Current_Row + 1;
                  end if;
               when 'L' =>
                  if Current_Column > 1 and then
                    Keypad_P2 (Current_Row, Current_Column - 1) /= '0' then
                     Current_Column := Current_Column - 1;
                  end if;
               when 'R' =>
                  if Current_Column < 5 and then
                    Keypad_P2 (Current_Row, Current_Column + 1) /= '0' then
                     Current_Column := Current_Column + 1;
                  end if;
               when others =>
                  raise Assert_Exception;
            end case;
            Current_Button_P2 := Keypad_P2 (Current_Row, Current_Column);
         end loop;
         Put (Current_Button_P2'Image);
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (File => Input_File);

end Main;
