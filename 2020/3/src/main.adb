with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Unchecked_Conversion;

procedure Main is
   type Slope_Cell is ('#', '.');
   for Slope_Cell use ('#' => 35, '.' => 46);
   for Slope_Cell'Size use 8;

   package Cell_IO is new Ada.Text_IO.Enumeration_IO (Slope_Cell);

   function Char_To_Slope_Cell is new Ada.Unchecked_Conversion
     (Source => Character, Target => Slope_Cell);

   package Slope_Row is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Slope_Cell);

   function Compare_Row
     (Left : Slope_Row.Vector; Right : Slope_Row.Vector) return Boolean with
      Inline
   is
   begin
      return
        Left.Length = Right.Length
        and then
        (for all I in Left.First_Index .. Left.Last_Index =>
           Left (I) = Right (I));
   end Compare_Row;

   package Slope_Grid is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Slope_Row.Vector,
      "="        => Compare_Row);

   function Parse_Row (S : String) return Slope_Row.Vector with
      Inline
   is
      Row : Slope_Row.Vector;
   begin
      for I in S'Range loop
         declare
            Cell : Slope_Cell := Char_To_Slope_Cell (S (I));
         begin
            Row.Append (Cell);
         end;
      end loop;
      return Row;
   end Parse_Row;

   Input_File : File_Type;
   Grid       : Slope_Grid.Vector;

   -- part 1 and 2
   Trees_Encountered : Natural  := 0;
   Current_Cell_Idx  : Positive := 1;

   type Pos_Arr is array (Positive range 1 .. 5) of Positive;
   To_The_Right  : constant Pos_Arr := (1, 3, 5, 7, 1);
   To_The_Bottom : constant Pos_Arr := (1, 1, 1, 1, 2);
   Accumulator   : Pos_Arr;
begin
   -- prepare data
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Line        : String           := Get_Line (File => Input_File);
         Current_Row : Slope_Row.Vector := Parse_Row (S => Line);
      begin
         Grid.Append (Current_Row);
      end;
   end loop;
   Close (File => Input_File);

   -- part 1 & 2
   for Step in To_The_Right'Range loop
      declare
         I : Positive := 1;
      begin
         Trees_Encountered := 0;
         Current_Cell_Idx  := 1;

         loop
            declare
               Next_Row : Positive := I + To_The_Bottom (Step);
            begin
               exit when Next_Row > Grid.Last_Index;

               if Current_Cell_Idx + To_The_Right (Step) >
                 Grid (Next_Row).Last_Index
               then
                  Current_Cell_Idx :=
                    Current_Cell_Idx + To_The_Right (Step) -
                    Grid (Next_Row).Last_Index;
               else
                  Current_Cell_Idx := Current_Cell_Idx + To_The_Right (Step);
               end if;

               if Grid (Next_Row) (Current_Cell_Idx) = '#' then
                  Trees_Encountered := Trees_Encountered + 1;
               end if;
               I := I + To_The_Bottom (Step);
            end;
         end loop;
         Put_Line
           ("Right" & To_The_Right (Step)'Image & ", down " &
            To_The_Bottom (Step)'Image & "=" & Trees_Encountered'Image);
         Accumulator (Step) := Trees_Encountered;
      end;

   end loop;

   declare
      Product : Long_Long_Integer := 1;
   begin
      for I of Accumulator loop
         Product := Product * Long_Long_Integer (I);
      end loop;
      Put_Line (Product'Image);
   end;

end Main;
