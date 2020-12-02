with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   type Parsed_Line is record
      Low      : Positive;
      High     : Positive;
      Symbol   : Character;
      Password : Unbounded_String;
   end record;

   function Parse_Line (s : String) return Parsed_Line is
      Result        : Parsed_Line;
      Low_Bound_Idx : Positive;
   begin
      for I in s'Range loop
         if s (I) = '-' then
            Result.Low    := Positive'Value (s (s'First .. I - 1));
            Low_Bound_Idx := I + 1;
            exit;
         end if;
      end loop;

      for I in Low_Bound_Idx .. s'Last loop
         if s (I) = ' ' then
            Result.High     := Positive'Value (s (Low_Bound_Idx .. I - 1));
            Result.Symbol   := s (I + 1);
            Result.Password := To_Unbounded_String (s (I + 4 .. s'Last));
            exit;
         end if;
      end loop;
      return Result;
   end Parse_Line;

   Input_File : File_Type;

   -- part 1
   Valid_Passwords : Natural := 0;
   -- part 2
   Valid_Passwords2 : Natural := 0;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Current_Line     : String      := Get_Line (File => Input_File);
         Current_Policy   : Parsed_Line := Parse_Line (Current_Line);
         Current_Password : String      := To_String (Current_Policy.Password);
         Current_Counter  : Natural     := 0;
      begin
         -- part 1
         for I in Current_Password'Range loop
            if Current_Password (I) = Current_Policy.Symbol then
               Current_Counter := Current_Counter + 1;
            end if;
         end loop;
         if Current_Counter >= Current_Policy.Low
           and then Current_Counter <= Current_Policy.High
         then
            Valid_Passwords := Valid_Passwords + 1;
         end if;

         -- part 2
         if
           (Current_Password (Current_Policy.Low) = Current_Policy.Symbol
            or else Current_Password (Current_Policy.High) =
              Current_Policy.Symbol)
           and then Current_Password (Current_Policy.Low) /=
             Current_Password (Current_Policy.High)
         then
            Valid_Passwords2 := Valid_Passwords2 + 1;
         end if;

      end;

   end loop;
   Close (File => Input_File);
   Put_Line (Valid_Passwords'Image);
   Put_Line(Valid_Passwords2'Image);
end Main;
