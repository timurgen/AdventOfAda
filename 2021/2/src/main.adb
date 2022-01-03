pragma Ada_2020;

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Main is

   type Command_Type is (forward, down, up);

   type Command_Line is record
      Command : Command_Type;
      Value   : Integer;
   end record;

   function Parse_Line (S : String) return Command_Line is
      Result : Command_Line;
   begin
      if Index (S, "forward", S'First) > 0 then
         Result.Command := forward;
      elsif Index (S, "down", S'First) > 0 then
         Result.Command := down;
      elsif Index (S, "up", S'First) > 0 then
         Result.Command := up;
      end if;
      Result.Value :=
        Integer'Value
          (S
             (Index (S, Result.Command'Image, S'First) +
                Result.Command'Image'Length + 1 ..
                  S'Last));
      return Result;
   end Parse_Line;

   Input_File : File_Type;

   Current_Y_Pos : Integer := 0; -- depth
   Current_X_Pos : Integer := 0;
   Part_1_Result : Integer;

   -- part 2
   Current_X2_Pos : Integer := 0;
   Current_Y2_Pos : Integer := 0;
   Aim            : Integer := 0;
   Part_2_Result  : Integer;
begin
   Open (Input_File, In_File, "input.txt");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Current_Command : Command_Line :=
           Parse_Line (Get_Line (File => Input_File));
      begin
         case Current_Command.Command is
            when forward =>
               Current_X_Pos := Current_X_Pos + Current_Command.Value;
               -- part 2
               Current_X2_Pos := Current_X2_Pos + Current_Command.Value;
               Current_Y2_Pos :=
                 Current_Y2_Pos + (Aim * Current_Command.Value);
            when down =>
               Current_Y_Pos := Current_Y_Pos + Current_Command.Value;
               -- part 2
               Aim := Aim + Current_Command.Value;
            when up =>
               Current_Y_Pos := Current_Y_Pos - Current_Command.Value;
               --part 2
               Aim := Aim - Current_Command.Value;

         end case;
      end;
   end loop;
   Close (File => Input_File);

   Part_1_Result := Current_X_Pos * Current_Y_Pos;
   Part_2_Result := Current_X2_Pos * Current_Y2_Pos;

   Put_Line ("part 1: " & Part_1_Result'Image);
   Put_Line ("part 2: " & Part_2_Result'Image);
end Main;
