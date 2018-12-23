with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   type Bot is record
      X : Integer;
      Y : Integer;
      Z : Integer;
      R : Natural;
   end record;
   type Bot_Ptr is access all Bot;

   package Bot_V_P is new Indefinite_Vectors (Positive, Bot_Ptr);

   function Manhattan_Distance (Left, Right : Bot) return Natural is
     (abs (Right.X - Left.X) + abs (Right.Y - Left.Y) +
      abs (Right.Z - Left.Z));

   function Parse_From_String (S : String) return Bot is
      X, Y, Z    : Integer;
      R          : Natural;
      Cursor     : Integer := S'First;
      Cursor_End : Integer := Cursor;
   begin
      for I in S'Range loop
         exit when S (I) = '<';
         Cursor := Cursor + 1;
      end loop;
      Cursor := Cursor + 1;

      for I in Cursor .. S'Last loop
         exit when S (I) = ',';
         Cursor_End := I;
      end loop;
      X := Integer'Value (S (Cursor .. Cursor_End));

      Cursor := Cursor_End + 2;
      for I in Cursor .. S'Last loop
         exit when S (I) = ',';
         Cursor_End := I;
      end loop;
      Y := Integer'Value (S (Cursor .. Cursor_End));

      Cursor := Cursor_End + 2;
      for I in Cursor .. S'Last loop
         exit when S (I) = '>';
         Cursor_End := I;
      end loop;
      Z := Integer'Value (S (Cursor .. Cursor_End));

      Cursor := Cursor_End + 2;
      for I in Cursor .. S'Last loop
         exit when S (I) = '=';
         Cursor := Cursor + 1;
      end loop;
      Cursor := Cursor + 1;
      R      := Natural'Value (S (Cursor .. S'Last));

      return (X, Y, Z, R);
   end Parse_From_String;

   function Get_Strongest_Bot (V : Bot_V_P.Vector) return Bot_Ptr is
      Current_Strength : Natural := 0;
      Current_Bot_Ptr  : Bot_Ptr;
   begin
      for I in V.First_Index .. V.Last_Index loop
         if V.Element (I).R > Current_Strength then
            Current_Strength := V.Element (I).R;
            Current_Bot_Ptr  := V.Element (I);
         end if;
      end loop;
      return Current_Bot_Ptr;
   end Get_Strongest_Bot;

   Bot_Grid   : Bot_V_P.Vector;
   Input_File : File_Type;
begin
   Open (Input_File, In_File, "input.txt");
   loop
      declare
         Line : String := Get_Line (Input_File);
         --Current_Bot: Bot := Parse_From_String(Line);
      begin
         Bot_Grid.Append (new Bot'(Parse_From_String (Line)));
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (Input_File);
   declare
      Stringest_Bot : Bot_Ptr := Get_Strongest_Bot (Bot_Grid);
      Counter       : Natural := 0;
   begin
      for I in Bot_Grid.First_Index .. Bot_Grid.Last_Index loop
         if Stringest_Bot.R >=
           Manhattan_Distance (Stringest_Bot.all, Bot_Grid.Element (I).all)
         then
            Counter := Counter + 1;
         end if;
      end loop;
      Put (Counter'Img);
   end;
end Main;
