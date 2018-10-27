with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Sequential_IO;

with Map; use Map;

procedure Main is

   Move_North_Char : Character := '^';
   Move_South_Char : Character := 'v';
   Move_East_Char  : Character := '>';
   Move_West_Char  : Character := '<';

   Char_Error : exception;

   Hous_Map : Visited_Houses.Map;

   package Char_IO is new Ada.Sequential_IO (Character);

   Input_File   : Char_IO.File_Type;
   Current_Char : Character;

   Current_X : Integer := 0;
   Current_Y : Integer := 0;

begin
   Char_IO.Open
     (File => Input_File, Mode => Char_IO.In_File, Name => "input.txt");

   -- Santa deliver first present at start point
   Hous_Map.Insert ((Current_X, Current_Y), new Integer'(1));

   loop
      Char_IO.Read (File => Input_File, Item => Current_Char);

      if Current_Char = Move_North_Char then
         Current_Y := Current_Y + 1;
      elsif Current_Char = Move_South_Char then
         Current_Y := Current_Y - 1;
      elsif Current_Char = Move_East_Char then
         Current_X := Current_X + 1;
      elsif Current_Char = Move_West_Char then
         Current_X := Current_X - 1;
      else
         raise Char_Error;
      end if;
      if not Hous_Map.Contains ((Current_X, Current_Y)) then
         Hous_Map.Insert ((Current_X, Current_Y), new Integer'(1));
      end if;

   end loop;
exception
   when Char_IO.End_Error =>
      Put_Line (Visited_Houses.Length (Hous_Map)'Img);
      if Char_IO.Is_Open (Input_File) then
         Char_IO.Close (Input_File);
      end if;

end Main;
