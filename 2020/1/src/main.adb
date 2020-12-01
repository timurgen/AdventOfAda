with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

procedure Main is
   package Integer_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Integer);
   Input_Numbers : Integer_List.Vector;
   Input_File    : File_Type;
begin
   -- load data
   Open (Input_File, In_File, "input.txt");
   loop
      exit when End_Of_File (File => Input_File);
      declare
         Line : Integer := Integer'Value (Get_Line (File => Input_File));
      begin
         Input_Numbers.Append (New_Item => Line);
      end;
   end loop;
   Close (File => Input_File);

   -- Part 1
   for I in Input_Numbers.First_Index .. Input_Numbers.Last_Index loop
      for J in Input_Numbers.First_Index + I .. Input_Numbers.Last_Index loop
         declare
            A       : Integer := Input_Numbers (I);
            B       : Integer := Input_Numbers (J);
            Product : Integer;
         begin
            if A + B = 2_020 then
               Product := A * B;
               Put_Line (Product'Image);
               exit;
            end if;
         end;
      end loop;
   end loop;

   -- Part 2
   for I in Input_Numbers.First_Index .. Input_Numbers.Last_Index loop
      for J in Input_Numbers.First_Index + I .. Input_Numbers.Last_Index loop
         for K in Input_Numbers.First_Index + J .. Input_Numbers.Last_Index
         loop
            declare
               A       : Integer := Input_Numbers (I);
               B       : Integer := Input_Numbers (J);
               C       : Integer := Input_Numbers (K);
               Product : Integer;
            begin
               if A + B + C = 2_020 then
                  Product := A * B * C;
                  Put_Line (Product'Image);
                  exit;
               end if;
            end;
         end loop;
      end loop;
   end loop;
end Main;
