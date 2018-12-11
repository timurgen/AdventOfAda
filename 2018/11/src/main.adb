with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Fuel_Cell;
   type Fuel_Cell_Ptr is access all Fuel_Cell;
   type Fuel_Cell_Arr is array (Positive range 1..300, Positive range 1..300) of Fuel_Cell_Ptr;

   type Fuel_Cell is new Integer;

   Input: constant Integer := 9798;
   Cells: Fuel_Cell_Arr;

   function Get_The_Hundred_Digit (I: Long_Integer) return Long_Integer is
      Z_Result: Long_Integer := 0;
   begin
      if I < 100 then
         return Z_Result;
      end if;
      return I / 100 mod 10;
   end Get_The_Hundred_Digit;

   Max_Power_3x3_Grid_Sum : Natural := 0;
   X_Of_Max_Region :Positive := 1;
   Y_Of_Max_Region : Positive := 1;
   --part 2
   Part_2_Max_Sum : Natural := 0;
   Part_2_Size: Natural := 0;
   Part_2X : Positive := 1;
   Part_2Y: Positive := 1;

begin
   for Y in Cells'Range(2) loop
      for X in Cells'Range(1) loop
         declare
            Rack_Id: Long_Integer := Long_Integer(X + 10);
            Power_Level : Long_Integer:= Rack_Id * Long_Integer(Y) + Long_Integer(Input);
            Fuel_Cell_Power: Fuel_Cell;
         begin
            Power_Level := Power_Level * Rack_Id;
            if Power_Level < 100 then
               Power_Level := 0;
            else
               Power_Level := Get_The_Hundred_Digit(Power_Level);
               Power_Level := Power_Level - 5;
               Fuel_Cell_power  := Fuel_Cell(Power_Level);
               Cells(X,Y) := new Fuel_Cell'(Fuel_Cell_power);
            end if;
         end;
      end loop;
   end loop;

   for Y in Cells'First(2)..Cells'Last(2)-2 loop
      for X in Cells'First(1)..Cells'Last(1)-2 loop
         declare
            Sum: Integer := 0;
         begin
            for I in X..X+2 loop
               for J in Y..Y+2 loop
                  Sum := Sum + Integer(Cells(I,J).all);
               end loop;
            end loop;
            if Sum > Max_Power_3x3_Grid_Sum then
               Max_Power_3x3_Grid_Sum := Sum;
               X_Of_Max_Region := X;
               Y_Of_Max_Region := Y;
            end if;
         end;
      end loop;
   end loop;
   Put_Line("Part 1: " & X_Of_Max_Region'Img & Y_Of_Max_Region'Img & Max_Power_3x3_Grid_Sum'Img);

   --yeah brute  force N^5
   for Idx in Cells'Range loop
      --Put_Line("Idx" & Idx'Img);
      for Y in Cells'First(2)..Cells'Last(2) - Idx loop
         for X in Cells'First(1)..Cells'Last(1) - Idx loop
            declare
               Sum: Integer := 0;
            begin
               for DY in Cells'First(2)..Idx loop
                  for DX in Cells'First(1)..Idx loop
                     Sum := Sum + Integer(Cells(X+DX, Y+DY).all);
                  end loop;
               end loop;
               if Sum > Part_2_Max_Sum then
                  Part_2_Max_Sum := Sum;
                  Part_2_Size := Idx;
                  Put_Line(X'Img & Y'Img & Idx'Img); -- add 1 to coords
               end if;
            end;
         end loop;
      end loop;
   end loop;

   Put_Line("Part 2: " & Part_2_Max_Sum'Img & Part_2_Size'Img);
end Main;
