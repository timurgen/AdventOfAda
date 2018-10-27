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

   Current_X_Santa : Integer := 0;
   Current_Y_Santa : Integer := 0;

   Current_X_Robosanta : Integer := 0;
   Current_Y_Robosanta : Integer := 0;

   Is_Santa_Moving : Boolean := True;

   procedure Update_Santa (Current_Char : in Character) is
   begin
      if Current_Char = Move_North_Char then
         Current_Y_Santa := Current_Y_Santa + 1;
      elsif Current_Char = Move_South_Char then
         Current_Y_Santa := Current_Y_Santa - 1;
      elsif Current_Char = Move_East_Char then
         Current_X_Santa := Current_X_Santa + 1;
      elsif Current_Char = Move_West_Char then
         Current_X_Santa := Current_X_Santa - 1;
      else
         raise Char_Error;
      end if;
      if not Hous_Map.Contains ((Current_X_Santa, Current_Y_Santa)) then
         Hous_Map.Insert ((Current_X_Santa, Current_Y_Santa), new Integer'(1));
      end if;

   end Update_Santa;

   procedure Update_Robosanta (Current_Char : in Character) is
   begin
      if Current_Char = Move_North_Char then
         Current_Y_Robosanta := Current_Y_Robosanta + 1;
      elsif Current_Char = Move_South_Char then
         Current_Y_Robosanta := Current_Y_Robosanta - 1;
      elsif Current_Char = Move_East_Char then
         Current_X_Robosanta := Current_X_Robosanta + 1;
      elsif Current_Char = Move_West_Char then
         Current_X_Robosanta := Current_X_Robosanta - 1;
      else
         raise Char_Error;
      end if;
      if not Hous_Map.Contains ((Current_X_Robosanta, Current_Y_Robosanta))
      then
         Hous_Map.Insert
           ((Current_X_Robosanta, Current_Y_Robosanta), new Integer'(1));
      end if;

   end Update_Robosanta;

begin
   Char_IO.Open
     (File => Input_File, Mode => Char_IO.In_File, Name => "input.txt");

   -- Santa and Robosanta deliver first present at start point
   Hous_Map.Insert ((Current_X_Santa, Current_Y_Santa), new Integer'(1));

   loop
      Char_IO.Read (File => Input_File, Item => Current_Char);

      if Is_Santa_Moving then
         Is_Santa_Moving := False;
         Update_Santa (Current_Char => Current_Char);
      else
         Is_Santa_Moving := True;
         Update_Robosanta (Current_Char => Current_Char);
      end if;

   end loop;
exception
   when Char_IO.End_Error =>
      Put_Line (Visited_Houses.Length (Hous_Map)'Img);
      if Char_IO.Is_Open (Input_File) then
         Char_IO.Close (Input_File);
      end if;

end Main;
