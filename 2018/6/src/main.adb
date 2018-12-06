with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   type Point is record
      X        : Natural;
      Y        : Natural;
      Infinity : Boolean := False;
   end record;

   type Point_Access is access all Point;

   function Is_Point_Equal (Left, Right : Point) return Boolean is (Left.X = Right.X and then Left.Y = Right.Y);

   package Point_Vector_P is new Indefinite_Vectors (Positive, Point, Is_Point_Equal);

   Point_List : Point_Vector_P.Vector;

   Bad_Point : exception;

   function Parse_Point (S : String) return Point is
      X, Y : Natural;
   begin
      for I in S'Range loop
         if S (I) = ',' then
            X := Natural'Value (S (S'First .. I - 1));
            Y := Natural'Value (S (I + 2 .. S'Last));
            return (X, Y, False);
         end if;
      end loop;
      raise Bad_Point;
   end Parse_Point;

   function Manhattan_Distance
     (Left, Right : Point) return Natural is
     (abs (Right.X - Left.X) + abs (Right.Y - Left.Y));

   Input_File     : File_Type;
   Max_Row_Number : Positive := 1;
   Max_Col_Number : Positive := 1;

   type Grid_T is array (Natural range <>, Natural range <>) of Point_Access;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Line          : String        := Get_Line (File => Input_File);
         Current_Point : aliased Point := Parse_Point (Line);
      begin
         Point_List.Append (Current_Point);
      end;
      exit when End_Of_File (File => Input_File);
   end loop;
   Close (File => Input_File);

   for I in Point_List.First_Index .. Point_List.Last_Index loop
      if Point_List (I).X > Max_Row_Number then
         Max_Row_Number := Point_List (I).X;
      end if;
      if Point_List (I).Y > Max_Col_Number then
         Max_Col_Number := Point_List (I).Y;
      end if;
   end loop;
   declare
      Grid         : Grid_T (0 .. Max_Row_Number + 1, 0 .. Max_Col_Number + 1)   := (others => (others => null));
      Result_Array : array (1 .. Max_Row_Number, 1 .. Max_Col_Number) of Natural := (others => (others => 0));

      Last_Known_Distance  : Natural;
      Current_Min_Distance : Natural := Natural'Last;
      Result_Num           : Natural := 0;
      Part2_Region_Size    : Natural := 0;
   begin
      for Y in Grid'Range (1) loop
         for X in Grid'Range loop
            Current_Min_Distance := Natural'Last;
            for Z in Point_List.First_Index .. Point_List.Last_Index loop
               Last_Known_Distance := Manhattan_Distance (Point_List (Z), (X, Y, False));
               if Last_Known_Distance < Current_Min_Distance then
                  if X = Grid'First or else X = Grid'Last or else Y = Grid'First (1) or else Y = Grid'Last (1) then
                     Point_List (Z).Infinity := True;
                     --null;
                  end if;
                  Grid (X, Y)          := new Point'(Point_List (Z).X, Point_List (Z).Y, Point_List (Z).Infinity);
                  Current_Min_Distance := Last_Known_Distance;
               elsif Last_Known_Distance = Current_Min_Distance then
                  Grid (X, Y) := null;
               end if;
            end loop;
         end loop;
      end loop;

      for Y in Grid'Range (1) loop
         for X in Grid'Range loop
            if Grid (X, Y) = null then
               --Put ("(....)");
               null;
            else
               if Grid (X, Y).Infinity then
                  --Put ("(.XX.)");
                  null;
               else
                  -- Put ("(" & Grid (X, Y).X'Img & Grid (X, Y).Y'Img & ")");
                  Result_Array (Grid (X, Y).X, Grid (X, Y).Y) := Result_Array (Grid (X, Y).X, Grid (X, Y).Y) + 1;
               end if;
            end if;
         end loop;
         --New_Line;
      end loop;
      for I in Result_Array'Range loop
         for J in Result_Array'Range (1) loop
            if Result_Array (I, J) > Result_Num then
               Result_Num := Result_Array (I, J);
            end if;
         end loop;
      end loop;
      Put_Line ("Part 1" & Result_Num'Img);

      --part 2
      for Y in Grid'Range (1) loop
         for X in Grid'Range loop
            declare
               Total_Distance : Natural := 0;
            begin
               for Z in Point_List.First_Index .. Point_List.Last_Index loop
                  Total_Distance := Total_Distance + Manhattan_Distance (Point_List (Z), (X, Y, False));
               end loop;
               if Total_Distance < 10000 then
                  --Put("#");
                  Part2_Region_Size := Part2_Region_Size + 1;
               else
                  --Put("X");
                  null;
               end if;
            end;
         end loop;
         --New_Line;
      end loop;
      Put_Line ("Part 2" & Part2_Region_Size'Img);
   end;

end Main;
