with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   Input_Str   : Unbounded_String;
   Current_Floor : Integer := 0;
   Index_Of_First_Basement_Floor: Positive;
   Is_Found: Boolean := False;
begin
   Input_Str := To_Unbounded_String (Get_Line);

   for I in 1 .. Length (Input_Str) loop
      if Element (Input_Str, I) = '(' then
         Current_Floor := Current_Floor + 1;
      elsif Element (Input_Str, I) = ')' then
         Current_Floor := Current_Floor - 1;
      end if;

      if Current_Floor = -1 and not Is_Found then
         Index_Of_First_Basement_Floor := I;
         Is_Found := True;
      end if;

   end loop;

   Put_Line (Integer'Image (Current_Floor));
   Put_Line(Positive'Image(Index_Of_First_Basement_Floor));
end Main;
