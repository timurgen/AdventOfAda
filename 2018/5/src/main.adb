with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;
procedure Main is
   Input_File : File_Type;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");

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
      Put_Line (Ada.Strings.Fixed.Trim(Line, Ada.Strings.Both)'Length'Img);
   end;
   Close (File => Input_File);
   -- part 2
   Open (File => Input_File, Mode => In_File, Name => "input.txt");

   declare
      Line   : String  := Get_Line (File => Input_File);
      Cursor : Positive := Line'First;
      Temp_Line: String(Line'First..Line'Last);
      Result_Arr: array(Character range 'a'..'z') of Integer;
      Min_Value: Integer := Integer'Last;
   begin
      for I in Character range 'a'..'z' loop
         Cursor := Line'First;
         Temp_Line := Line;
         loop
            exit when Cursor >= Temp_Line'Last;
            if Temp_Line(Cursor) = I or else Temp_Line(Cursor) = To_Upper(I) then
               Ada.Strings.Fixed.Delete (Temp_Line, Cursor, Cursor);
               if Cursor > 1 then
                  Cursor := Cursor - 1;
               end if;
            else
               Cursor := Cursor +1;
            end if;
         end loop;
         Cursor := Temp_Line'First;
         declare
            Trimmed_Line: String := Ada.Strings.Fixed.Trim(Temp_Line, Ada.Strings.Both);
         begin
            loop
               exit when Cursor >= Trimmed_Line'Last;
               if
                 (Is_Lower (Trimmed_Line (Cursor)) and then Is_Upper (Trimmed_Line (Cursor + 1))
                  and then Trimmed_Line (Cursor) = To_Lower (Trimmed_Line (Cursor + 1)))
                 or else
                   (Is_Upper (Trimmed_Line (Cursor)) and then Is_Lower (Trimmed_Line (Cursor + 1))
                    and then Trimmed_Line (Cursor) = To_Upper (Trimmed_Line (Cursor + 1)))
               then
                  Ada.Strings.Fixed.Delete (Trimmed_Line, Cursor, Cursor + 1);
                  if Cursor > 1 then
                     Cursor := Cursor - 1;
                  end if;

               else
                  Cursor := Cursor + 1;
               end if;
            end loop;
            Result_Arr(I) := Ada.Strings.Fixed.Trim(Trimmed_Line, Ada.Strings.Both)'Length;
            Put_Line (I'Img & Ada.Strings.Fixed.Trim(Trimmed_Line, Ada.Strings.Both)'Length'Img);
         end;

      end loop;
      for I in Result_Arr'Range loop
         if Result_Arr(I) < Min_Value then
            Min_Value := Result_Arr(I);
         end if;
      end loop;
      Put_Line(Min_Value'Img);


   end;
   Close (File => Input_File);
end Main;
