with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure Main is
   use Ada;

   type Char_Counter_Arr is array (Character range 'a' .. 'z') of Natural;

   Input_File    : Text_IO.File_Type;
   Two_Letters   : Natural := 0;
   Three_Letters : Natural := 0;

   -- part 2
   package String_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);
   Input_Strings : String_List.Vector;

   function Is_Exactly_One_Char_Diff (A, B : String) return Boolean is
      Levenstein_Distance : Natural := 0;
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            Levenstein_Distance := Levenstein_Distance + 1;
         end if;

         exit when Levenstein_Distance > 1;
      end loop;
      return Levenstein_Distance = 1;
   end Is_Exactly_One_Char_Diff;

begin
   Text_IO.Open
     (File => Input_File,
      Mode => Text_IO.In_File,
      Name => "input.txt");
   loop
      exit when Text_IO.End_Of_File (File => Input_File);
      declare
         Line         : String := Text_IO.Get_Line (File => Input_File);
         Char_Counter : Char_Counter_Arr := (others => 0);
      begin
         for I in Line'Range loop
            Char_Counter (Line (I)) := Char_Counter (Line (I)) + 1;
         end loop;

         for I in Char_Counter'Range loop
            if Char_Counter (I) = 2 then
               Two_Letters := Two_Letters + 1;
               exit;
            end if;
         end loop;

         for I in Char_Counter'Range loop
            if Char_Counter (I) = 3 then
               Three_Letters := Three_Letters + 1;
               exit;
            end if;
         end loop;
         -- part 2 populate list
         Input_Strings.Append (Line);
      end;
   end loop;
   Text_IO.Close (Input_File);
   --part 2
   for I in Input_Strings.First_Index .. Input_Strings.Last_Index loop
      for J in Input_Strings.First_Index + I .. Input_Strings.Last_Index loop
         declare
            A : String := Input_Strings (I);
            B : String := Input_Strings (J);
         begin
            if Is_Exactly_One_Char_Diff (A => A, B => B) then
               for K in A'Range loop
                  if A(K) = B(K) then
                     Text_IO.Put(A(K));
                  end if;

               end loop;
               Text_IO.New_Line;
            end if;

         end;
      end loop;
   end loop;

   -- output
   Text_IO.Put_Line
     (Natural'Image (Two_Letters) &
        " " &
        Natural'Image (Three_Letters) &
        " = " &
        Natural'Image (Two_Letters * Three_Letters));
end Main;
