with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure Main is
   Puzzle_Input : constant Integer := 768071;

   type Recipe_Score is range 0 .. 9;

   type Recipe;
   type Recipe_Ptr is access all Recipe;

   type Recipe is record
      Score    : Recipe_Score;
      Previous : Recipe_Ptr;
      Next     : Recipe_Ptr;
   end record;

   procedure Print_Board (Board, Elf1, Elf2, Last : Recipe_Ptr) is
      Current : Recipe_Ptr := Board;
   begin
      loop
         if Current = Elf1 then
            Put ("(");
         elsif Current = Elf2 then
            Put ("[");
         else
            Put (" ");
         end if;
         Put (Recipe_Score'Image (Current.Score) (2));
         if Current = Elf1 then
            Put (")");
         elsif Current = Elf2 then
            Put ("]");
         else
            Put (" ");
         end if;
         Current := Current.Next;
         exit when Current = Last.Next;
      end loop;
      New_Line;
   end Print_Board;

   function Get_Score (Ptr : Recipe_Ptr; Hops : Natural) return Recipe_Score is
      Temp : Recipe_Ptr := Ptr;
   begin
      if Hops = 0 then
         return Temp.Score;
      end if;

      for I in 1 .. Hops loop
         Temp := Temp.Previous;
      end loop;
      return Temp.Score;
   end Get_Score;

   Elf_1_Current_Recipe : Recipe_Ptr;
   Elf_2_CUrrent_Recipe : Recipe_Ptr;
   Board                : Recipe_Ptr := new Recipe;
   Last_Recipe          : Recipe_Ptr := Board;
   Iterations           : Natural    := 1;

   -- part 2
   Last_Puzzle_Digit      : Recipe_Score := Recipe_Score (Puzzle_Input mod 10);
   Puzzle_Input_As_String : String       := Trim (Integer'Image (Puzzle_Input), Ada.Strings.Left);
   Puzzle_Length          : Positive     := Puzzle_Input_As_String'Length;
   Recipes                : Positive     := 2;

begin
   --initial condition
   Board.Score          := 3;
   Board.Previous       := Board;
   Board.Next           := new Recipe'(Score => 7, Previous => Board, Next => Board);
   Elf_1_Current_Recipe := Board;
   Elf_2_CUrrent_Recipe := Board.Next;
   Last_Recipe          := Board.Next;

   --Print_Board (Board, Elf_1_Current_Recipe, Elf_2_CUrrent_Recipe, Last_Recipe);

   loop
      -- exit when Iterations = Puzzle_Input + 10; -- part 1 exit condition
      Iterations := Iterations + 1;
      declare
         Sum_Score_Curr_Recipes : Natural := Natural (Elf_1_Current_Recipe.Score + Elf_2_CUrrent_Recipe.Score);
      begin
         if Sum_Score_Curr_Recipes > 9 then
            Last_Recipe.Next := new Recipe'(Recipe_Score (Sum_Score_Curr_Recipes / 10), Last_Recipe, Last_Recipe.Next);
            Last_Recipe      := Last_Recipe.Next;
            Last_Recipe.Next := Board;
            Recipes          := Recipes + 1;
            --part 2 check
            if
              (for all I in Puzzle_Input_As_String'Range =>
                 Recipe_Score'Value ((1 => Puzzle_Input_As_String (I))) =
                 Get_Score (Ptr => Last_Recipe, Hops => Puzzle_Length - I))
            then
               Put_Line ("Exit after " & Natural'Image (Recipes - Puzzle_Length) & " recipes");
               --Print_Board(Board, Elf_1_Current_Recipe, Elf_2_CUrrent_Recipe, Last_Recipe);
               exit;
            end if;
         end if;
         Last_Recipe.Next := new Recipe'(Recipe_Score (Sum_Score_Curr_Recipes mod 10), Last_Recipe, Last_Recipe.Next);
         Last_Recipe      := Last_Recipe.Next;
         Last_Recipe.Next := Board;
         Recipes          := Recipes + 1;
         --part 2 check
         if
           (for all I in Puzzle_Input_As_String'Range =>
              Recipe_Score'Value ((1 => Puzzle_Input_As_String (I))) =
              Get_Score (Ptr => Last_Recipe, Hops => Puzzle_Length - I))
         then
            Put_Line ("Exit after " & Natural'Image (Recipes - Puzzle_Length) & " recipes");
            --Print_Board(Board, Elf_1_Current_Recipe, Elf_2_CUrrent_Recipe, Last_Recipe);
            exit;
         end if;

         --new current recipes
         for Elf1 in 1 .. Elf_1_Current_Recipe.Score + 1 loop
            Elf_1_Current_Recipe := Elf_1_Current_Recipe.Next;
         end loop;
         for Elf2 in 1 .. Elf_2_CUrrent_Recipe.Score + 1 loop
            Elf_2_CUrrent_Recipe := Elf_2_CUrrent_Recipe.Next;
         end loop;

         --Print_Board(Board, Elf_1_Current_Recipe, Elf_2_CUrrent_Recipe, Last_Recipe);

      end;
   end loop;
   for I in 0 .. Puzzle_Input loop
      Last_Recipe := Last_Recipe.Next;
   end loop;

   for I in 1 .. 10 loop
      Put (Recipe_Score'Image (Last_Recipe.Score));
      Last_Recipe := Last_Recipe.Next;
   end loop;

end Main;
