with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure Main is

   function Count_Vowels (S : String) return Integer is
      Result       : Integer := 0;
      Vowels       : String  := "aeiou";
      Current_Char : Character;
   begin
      for I in 1 .. S'Length loop
         Current_Char := S (I);
         if Index (Vowels, "" & Current_Char) > 0 then
            Result := Result + 1;
         end if;

      end loop;
      return Result;
   end Count_Vowels;

   function Is_String_Nice_V1 (S : String) return Boolean is
      Has_At_Least_3_Vowels : Boolean := False;
      Has_Repeated_Chars    : Boolean := False;
   begin

      for I in S'Range loop
         if I < S'Length and then S (I) = S (I + 1) then
            Has_Repeated_Chars := True;
            exit;
         end if;
      end loop;

      if Count_Vowels (S) >= 3 then
         Has_At_Least_3_Vowels := True;
      end if;

      return Has_At_Least_3_Vowels and Has_Repeated_Chars;
   end Is_String_Nice_V1;

   function Is_String_Nice_V2 (S : String) return Boolean is
      Complies_With_Rule_One : Boolean := False;
      Complies_With_Rule_Two : Boolean := False;
      Pair_Of_Letters        : String (1 .. 2);
   begin
      for I in S'Range loop
         if I + 2 <= S'Length and then S (I) = S (I + 2) then
            Complies_With_Rule_One := True;
         end if;

         if I + 2 < S'Length then
            Pair_Of_Letters := S (I .. I + 1);
            if Index (S (I + 2 .. S'Last), Pair_Of_Letters) > 0 then
               Complies_With_Rule_Two := True;
            end if;

         end if;

      end loop;

      return Complies_With_Rule_One and Complies_With_Rule_Two;
   end Is_String_Nice_V2;

   function Is_String_Naughty_V1 (S : String) return Boolean is
      Ab : String := "ab";
      Bc : String := "cd";
      Pq : String := "pq";
      Xy : String := "xy";

   begin
      if Index (S, Ab) > 0 or else Index (S, Bc) > 0 or else Index (S, Pq) > 0 or else Index (S, Xy) > 0 then
         return True;
      end if;

      if Count_Vowels (S) < 3 then
         return True;
      end if;

      return False;
   end Is_String_Naughty_V1;

   Input_File    : File_Type;
   Is_Nice_V1    : Integer := 0;
   Is_Naughty_V1 : Integer := 0;

   Is_Nice_V2    : Integer := 0;
   Is_Naughty_V2 : Integer := 0;

   Something_Weird : exception;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");

   loop
      declare
         Line : String := Get_Line (Input_File);
      begin
-- part 1 of 2015/5
--         if not Is_String_Naughty_V1 (Line) and then Is_String_Nice_V1 (Line) then
--           Is_Nice_V1 := Is_Nice_V1 + 1;
--         end if;
--         if Is_String_Naughty_V1 (Line) or else not Is_String_Nice_V1 (Line) then
--            Is_Naughty_V1 := Is_Naughty_V1 + 1;
--         end if;
--part 2 of 2015/5
         if Is_String_Nice_V2 (Line) then
            Is_Nice_V2 := Is_Nice_V2 + 1;
         end if;
         if not Is_String_Nice_V2 (Line) then
            Is_Naughty_V2 := Is_Naughty_V2 + 1;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      if Is_Open (Input_File) then
         Put_Line ("Nice: " & Is_Nice_V2'Img);
         Put_Line ("Naughty: " & Is_Naughty_V2'Img);
         Close (Input_File);
      end if;

end Main;
