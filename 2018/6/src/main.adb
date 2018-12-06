with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   type Point is record
      X : Natural;
      Y : Natural;
   end record;

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
            return (X, Y);
         end if;
      end loop;
      raise Bad_Point;
   end Parse_Point;

   function Manhattan_Distance (Left, Right : Point) return Natural is
     (abs (Right.X - Left.X) + abs (Right.Y - Right.X));

   Input_File     : File_Type;
   Max_Row_Number : Positive := 1;
   Max_Col_Number : Positive := 1;
begin
   Put_Line (Integer'Image (2 - 2));
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Line          : String := Get_Line (File => Input_File);
         Current_Point : Point  := Parse_Point (Line);
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

   for X in 0 .. Max_Row_Number + 1 loop
      for Y in 0 .. Max_Col_Number + 1 loop
         null;
      end loop;

   end loop;

end Main;
