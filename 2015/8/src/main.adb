with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
   Input_File    : File_Type;
   Code_Chars    : Natural := 0;
   Memory_Chars  : Natural := 0;
   Encoded_Chars : Natural := 0;
   function Get_Code_Chars (S : String) return Natural is
   begin
      return S'Length;
   end Get_Code_Chars;

   function Get_Memory_Chars (S : String) return Natural is
      Result  : Natural := 0;
      Cur_Pos : Integer := S'First;
   begin
      while Cur_Pos <= S'Last loop
         if Cur_Pos /= S'First and then Cur_Pos /= S'Last then
            if S (Cur_Pos) = '\' and then S (Cur_Pos + 1) = '\' then
               Cur_Pos := Cur_Pos + 1;
            elsif S (Cur_Pos) = '\' and then S (Cur_Pos + 1) = '"' then
               Cur_Pos := Cur_Pos + 1;
            elsif S (Cur_Pos) = '\' and then S (Cur_Pos + 1) = 'x' then
               Cur_Pos := Cur_Pos + 3;
            end if;
            Result := Result + 1;
         end if;
         Cur_Pos := Cur_Pos + 1;

      end loop;
      return Result;
   end Get_Memory_Chars;

   function Get_Encoded (S : String) return Natural is
      Result : Natural := 0;
   begin
      for I in S'Range loop
         if I = S'First  or else I = S'Last then
            Result := Result + 3;
         elsif S(I) = '"' or else S(I) = '\' then
            Result := Result + 2;
         else
            Result := Result +1;
         end if;

      end loop;

      return Result;
   end Get_Encoded;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   Put("(");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Line                  : String  := Get_Line (Input_File);
         Current_Code          : Natural := 0;
         Current_Memory        : Natural := 0;
         Current_Encoded_Chars : Natural := 0;
      begin
         Current_Code          := Get_Code_Chars (Line);
         Current_Memory        := Get_Memory_Chars (Line);
         Current_Encoded_Chars := Get_Encoded (Line);
         Put(" " &Current_Encoded_Chars'Img & " +");

         Code_Chars    := Code_Chars + Current_Code;
         Memory_Chars  := Memory_Chars + Current_Memory;
         Encoded_Chars := Encoded_Chars + Current_Encoded_Chars;
      end;
   end loop;
   Put(")");
   New_Line;
   Close (File => Input_File);
   Put_Line ("Code :" & Natural'Image (Code_Chars));
   Put_Line ("Memory :" & Natural'Image (Memory_Chars));
   Put_Line ("Encoded :" & Natural'Image (Encoded_Chars));
end Main;
