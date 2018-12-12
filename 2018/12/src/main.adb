with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is
   package IIO is new Integer_IO (Integer);

   type Pots_Arr is array (-10 .. 2048, 0 .. 1) of Boolean;
   type Pots_Arr_Ptr is access all Pots_Arr;

   type Rule is record
      L2     : Boolean;
      L1     : Boolean;
      C      : Boolean;
      R1     : Boolean;
      R2     : Boolean;
      Result : Boolean;
   end record;

   package Rule_V_P is new Indefinite_Vectors (Positive, Rule);

   function Init_Pots (S : String) return Pots_Arr_Ptr is
      Pots_Array : Pots_Arr_Ptr := new Pots_Arr'(others => (others => False));
   begin
      for I in S (S'First + 15 .. S'Last)'Range loop
         if S (I) = '#' then
            Pots_Array (I - 16, 0) := True;
         end if;

      end loop;

      return Pots_Array;
   end Init_Pots;

   procedure Print_Pots (Pots : Pots_Arr; Counter : Integer) is
   begin
      for I in Pots'Range (2) loop
         IIO.Put (Item => Counter, Width => 3);
         Put (": ");
         for J in -3 .. 175 loop
            if Pots (J, I) = False then
               Put (".");
            else
               Put ("#");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Pots;

   function Parse_Rule (S : String) return Rule is
      L2, L1, C, R1, R2, Result : Boolean;
   begin
      if S (S'First) = '.' then
         L2 := False;
      else
         L2 := True;
      end if;
      if S (S'First + 1) = '.' then
         L1 := False;
      else
         L1 := True;
      end if;
      if S (S'First + 2) = '.' then
         C := False;
      else
         C := True;
      end if;
      if S (S'First + 3) = '.' then
         R1 := False;
      else
         R1 := True;
      end if;
      if S (S'First + 4) = '.' then
         R2 := False;
      else
         R2 := True;
      end if;
      if S (S'Last) = '.' then
         Result := False;
      else
         Result := True;
      end if;
      return (L2, L1, C, R1, R2, Result);
   end Parse_Rule;

   function Compute_Sum_Of_First_Row (P : Pots_Arr_Ptr) return Integer is
      Result : Integer := 0;
   begin
      for I in P'Range (1) loop
         --Put_Line(I'Img & P(I, P'Last(2))'Img);
         if P (I, P'First (2)) = True then
            Result := Result + I;
         end if;
      end loop;
      return Result;
   end Compute_Sum_Of_First_Row;

   Pots       : Pots_Arr_Ptr;
   Rules      : Rule_V_P.Vector;
   Input_File : File_Type;
   Counter    : Integer := 0;
begin
   Open (Input_File, In_File, "input.txt");
   declare
      Initial_State_Str : String := Get_Line (Input_File);
      Next_Empty_Line   : String := Get_Line (Input_File);
   begin
      Pots := Init_Pots (Initial_State_Str);
      loop
         Rules.Append (Parse_Rule (Get_Line (Input_File)));
         exit when End_Of_File (Input_File);
      end loop;
   end;
   Close (Input_File);
   -- 20 iterations for part 1
   --part 2 with 50_000_000_000 iterations seems to be impossible to compute stright forward
   while Counter < 302 loop
      for Step in Pots'First (2) .. Pots'Last (2) loop
         Counter := Counter + 1;
         for Pot in Pots'First (1) + 2 .. Pots'Last (1) - 2 loop
            if Step /= Pots'Last (2) then
               Pots (Pot, Step + 1) := False;
            else
               Pots (Pot, Step - 1) := False;
            end if;
            for I in Rules.First_Index .. Rules.Last_Index loop

               if Rules.Element (I).L2 = Pots (Pot - 2, Step)
                 and then Rules.Element (I).L1 = Pots (Pot - 1, Step)
                 and then Rules.Element (I).C = Pots (Pot, Step)
                 and then Rules.Element (I).R1 = Pots (Pot + 1, Step)
                 and then Rules.Element (I).R2 = Pots (Pot + 2, Step)
               then
                  if Step /= Pots'Last (2) then
                     Pots (Pot, Step + 1) := Rules.Element (I).Result;
                  else
                     Pots (Pot, Step - 1) := Rules.Element (I).Result;
                  end if;
                  exit;
               end if;
            end loop;
         end loop;
         --Print_Pots (Pots.all, Integer(Counter));
      end loop;
   end loop;
   Put_Line ("Result: " & Integer'Image (Compute_Sum_Of_First_Row (Pots)));
   --100 iterations = 9152
   --102 iterations = 9346
   --diff/2 = 97
   -- 200 = 17480
   -- 202 = 17640
   --diff/2 = 80
   -- 300 = 25480
   --302 = 25640
   --diff/2 = 80 - Eurika!
   Put_Line
     ("Result: " &
        Long_Long_Integer'Image
        (Long_Long_Integer (Compute_Sum_Of_First_Row (Pots)) +
         ((50000000000 - 302) * 80)));
end Main;
