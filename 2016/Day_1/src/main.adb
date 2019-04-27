with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Main is

   type Turn_Direction is (LEFT, RIGHT, UNDEFINED);
   type Directions is (S, N, W, E);
   Bad_Direction : exception;

   type Movement_Event is record
      Turn_90_Degree_To : Turn_Direction;
      Blocks_To_Go      : Positive;
   end record;

   type Movement_Event_Array is array (Positive range <>) of Movement_Event;

   function Load_Data_From_String (S : String) return Movement_Event_Array is
      function Count_Tokens (S : String) return Natural is
         Token_Number : Natural := 0;
      begin
         for I in S'Range loop
            if S (I) = ',' then
               Token_Number := Token_Number + 1;
            end if;
         end loop;
         return Token_Number + 1;
      end Count_Tokens;

      Size              : Natural  := Count_Tokens (S);
      Current_Token     : Positive := 1;
      Current_Direction : Turn_Direction;
      Current_Blocks    : Positive;
      Events_Array      : Movement_Event_Array (1 .. Size);
      Cursor            : Positive := S'First;
      Cursos_End        : Positive := Cursor;
   begin
      loop
         Current_Direction := (if S (Cursor) = 'R' then RIGHT elsif S (Cursor) = 'L' then LEFT else UNDEFINED);

         if Current_Direction = UNDEFINED then
            raise Bad_Direction;
         end if;

         Cursor     := Cursor + 1;
         Cursos_End := Cursor;

         loop
            Cursos_End := Cursos_End + 1;
            exit when Cursos_End >= S'Last or else S (Cursos_End) = ',';
         end loop;

         Current_Blocks               := Positive'Value (S (Cursor .. Cursos_End - 1));
         Events_Array (Current_Token) := (Current_Direction, Current_Blocks);
         Current_Token                := Current_Token + 1;
         Cursor                       := Cursos_End + 2;

         exit when Cursor > S'Last;
      end loop;
      return Events_Array;
   end Load_Data_From_String;

   function Change_Direction (Dir : Directions; Turn : Turn_Direction) return Directions is
     (case Dir is when N => (if Turn = LEFT then W elsif Turn = RIGHT then E else raise Bad_Direction),
        when S           => (if Turn = LEFT then E elsif Turn = RIGHT then W else raise Bad_Direction),
        when W           => (if Turn = LEFT then S elsif Turn = RIGHT then N else raise Bad_Direction),
        when E           => (if Turn = LEFT then N elsif Turn = RIGHT then S else raise Bad_Direction),
        when others      => raise Bad_Direction);

   -- part 2
   type Coordinates is record
      X : Integer;
      Y : Integer;
   end record;

   type Integer_Acc is access Integer;

   function Get_Hash (Id : Coordinates) return Hash_Type is
      Hash : Integer := 0;
   begin
      Hash := ((Id.X + Id.Y) * (Id.X + Id.Y + 1)) / 2 + Id.Y;
      return Hash_Type'Val (Hash);
   end Get_Hash;

   package Visited_Coordinates is new Ada.Containers.Hashed_Maps (Key_Type => Coordinates, Element_Type => Integer_Acc,
      Hash                                                                 => Get_Hash, Equivalent_Keys => "=");

   Already_Visited : exception;
   procedure Store_Path (M : in out Visited_Coordinates.Map; X1, Y1, X2, Y2 : Integer) is
      Min_X : Integer := Integer'Min (X1, X2);
      Max_X : Integer := Integer'Max (X1, X2);
      Min_Y : Integer := Integer'Min (Y1, Y2);
      Max_Y : Integer := Integer'Max (Y1, Y2);
   begin
      for X in Min_X .. Max_X loop
         for Y in Min_Y .. Max_Y loop
            if not M.Contains ((X, Y)) then
               M.Insert ((X, Y), new Integer'(1));
            else
               Put_Line ("part 2:" & Natural'Image (abs X) & " +" & Natural'Image (abs Y));
               raise Already_Visited;
            end if;
         end loop;
      end loop;
   end Store_Path;

   Visited_Map               : Visited_Coordinates.Map;
   First_Visited_Twise_Found : Boolean := False;

   Input_File        : File_Type;
   Current_X         : Integer    := 0;
   Current_Y         : Integer    := 0;
   Current_Direction : Directions := N;
begin
   Open (Input_File, In_File, "test2.txt");
   declare
      Events : Movement_Event_Array := Load_Data_From_String (Get_Line (Input_File));
   begin
      for I in Events'Range loop
         Current_Direction := Change_Direction (Current_Direction, Events (I).Turn_90_Degree_To);
         case Current_Direction is
            when S =>
               Store_Path (Visited_Map, Current_X, Current_Y, Current_X, Current_Y - Events (I).Blocks_To_Go);
               Current_Y := Current_Y - Events (I).Blocks_To_Go;
            when N =>
               Store_Path (Visited_Map, Current_X, Current_Y, Current_X, Current_Y + Events (I).Blocks_To_Go);
               Current_Y := Current_Y + Events (I).Blocks_To_Go;
            when W =>
               Store_Path (Visited_Map, Current_X, Current_Y, Current_X - Events (I).Blocks_To_Go, Current_Y);
               Current_X := Current_X - Events (I).Blocks_To_Go;
            when E =>
               Store_Path (Visited_Map, Current_X, Current_Y, Current_X + Events (I).Blocks_To_Go, Current_Y);
               Current_X := Current_X + Events (I).Blocks_To_Go;
         end case;
      end loop;
      Put_Line ("Part 1 " & Natural'Image (abs Current_X) & " +" & Natural'Image (abs Current_Y));
   end;
   Close (Input_File);
end Main;
