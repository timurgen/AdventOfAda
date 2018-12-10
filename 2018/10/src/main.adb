with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   type Point;
   type Point_Ptr is access Point;

   type Point is record
      X: Integer;
      Y: Integer;
      X_Vel: Integer;
      Y_Vel: Integer;
   end record;

   package Point_V_P is new Indefinite_Vectors(Positive, Point_Ptr);
   Point_List: Point_V_P.Vector;

   function Parse_Point(S: String) return Point is
      X: Integer;
      Y: Integer;
      X_Vel: Integer;
      Y_Vel: Integer;

      Cursor_Next: Positive;

      function Parse_Int (S: String; Cursor_End: out Positive) return Integer is
         Cursor: Positive:= S'First;
      begin
         Cursor_End := Cursor;
         while S(Cursor) not in '0'..'9'|'-' loop
            Cursor := Cursor + 1;
         end loop;
         Cursor_End := Cursor + 1;
         while S(Cursor_End) in '0'..'9' loop
            Cursor_End := Cursor_End + 1;
         end loop;
         return Integer'Value(S(Cursor..Cursor_End-1));
      end Parse_Int;
   begin
      X := Parse_Int(S, Cursor_Next);
      Y := Parse_Int(S(Cursor_Next..S'Last), Cursor_Next);
      X_Vel := Parse_Int(S(Cursor_Next..S'Last), Cursor_Next);
      Y_Vel := Parse_Int(S(Cursor_Next..S'Last), Cursor_Next);
      return (X,Y,X_Vel,Y_Vel);
   end Parse_Point;

   function Compute_Points_Height(P_List: Point_V_P.Vector) return Natural is
      Lower_Bound: Integer := Integer'Last;
      Higher_bound: Integer := Integer'First;
   begin
      for I in P_List.First_Index..P_List.Last_Index loop
         if P_List(I).Y < Lower_Bound then
            Lower_Bound := P_List(I).Y;
         elsif P_List(I).Y > Higher_bound then
            Higher_bound := P_List(I).Y;
         end if;
      end loop;

      return abs (Higher_bound - Lower_Bound);
   end Compute_Points_Height;

   procedure Run_Points (P_List: Point_V_P.Vector) is
   begin
      for I in P_List.First_Index..P_List.Last_Index loop
         P_List(I).X := P_List(I).X + P_List(I).X_Vel;
         P_List(I).Y := P_List(I).Y + P_List(I).Y_Vel;
      end loop;
   end Run_Points;

   procedure Render_Points (P_list: Point_V_P.Vector) is
   begin
      for P in P_list.First_Index..P_list.Last_Index loop
         Put(P_list(P).X'Img & ",");
      end loop;
      New_Line;
      for P in P_list.First_Index..P_list.Last_Index loop
         Put(P_list(P).Y'Img & ",");
      end loop;
   end Render_Points;

   Input_File: File_Type;
   Counter : Natural := 0;
begin
   Open(Input_File, In_File, "input.txt");
   loop
      declare
         Line: String:= Get_Line(Input_File);
         P: Point := Parse_Point(Line);
         P_Ptr : Point_Ptr := new Point'(P.X,P.Y,P.X_Vel,P.Y_Vel);
      begin
         Point_List.Append(P_Ptr);
      end;
      exit when End_Of_File(Input_File);
   end loop;
   Close(Input_File);

   while Compute_Points_Height(Point_List) > 10 loop
      Counter := Counter + 1;
      Run_Points(Point_List);
   end loop;

   Render_Points(Point_List);
   Put_Line("Part 2" & Natural'Image(Counter));
end Main;
