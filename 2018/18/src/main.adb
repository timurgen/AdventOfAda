with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Acre_Status is (OPEN_GROUND, TREES, LUMBERYARD, UNDEFINED);

   type Acre;
   type Acre_Ptr is access all Acre;

   type Acre_Yard is array (1 .. 50, 1 .. 50) of Acre_Ptr;

   type Acre is record
      State : Acre_Status;
   end record;

   function In_Bounds (DX, DY : Integer; Main_Yard : Acre_Yard) return Boolean
   is

   begin
      return DX >= Main_Yard'First (1) and then DX <= Main_Yard'Last (1)
        and then DY >= Main_Yard'First (2) and then DY <= Main_Yard'Last (2);
   end In_Bounds;

   procedure Print_Yard (A : Acre_Yard) is
   begin
      for Y in A'Range (2) loop
         for X in A'Range (1) loop
            case A (X, Y).State is
               when OPEN_GROUND =>
                  Put (".");
               when TREES =>
                  Put ("|");
               when LUMBERYARD =>
                  Put ("#");
               when others =>
                  null;
            end case;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Yard;

   Main_Yard : Acre_Yard := (others => (others => null));
   Temp_Yard : Acre_Yard :=
     (others => (others => new Acre'(State => UNDEFINED)));
   Input_File   : File_Type;
   Line_Counter : Positive := 1;
begin

   -- init
   Open (Input_File, In_File, "input.txt");
   loop
      declare
         Line          : String      := Get_Line (Input_File);
         Current_State : Acre_Status := UNDEFINED;
      begin
         for I in Line'Range loop
            Current_State :=
              (if Line (I) = '.' then OPEN_GROUND
               elsif Line (I) = '|' then TREES
               elsif Line (I) = '#' then LUMBERYARD else UNDEFINED);
            Main_Yard (I, Line_Counter) := new Acre'(State => Current_State);
         end loop;
         Line_Counter := Line_Counter + 1;
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (Input_File);
   --Print_Yard(Main_Yard);

   -- part 1 just iterate 10 iterations and see answer,
   -- part 2 run some hundred iterations, find periodic behavior, in my case starting from 457
   -- getting answer 457 + (1000000000 - 457) % 28  -> next turn with same anser as after 1b iterations
   -- It actually gave me error number, I had to add 1 (off by one error?)
   for Turn in 1 .. 10_000 loop
      if 1 = 1 then -- debug
         Put ("Turn: " & Turn'Img & " : ");
         declare
            Wooded_Acres       : Natural := 0;
            Lumberyarded_Acres : Natural := 0;
         begin
            for Y in Main_Yard'Range (2) loop
               for X in Main_Yard'Range (1) loop
                  if Main_Yard (X, Y).State = TREES then
                     Wooded_Acres := Wooded_Acres + 1;
                  elsif Main_Yard (X, Y).State = LUMBERYARD then
                     Lumberyarded_Acres := Lumberyarded_Acres + 1;
                  end if;
               end loop;
            end loop;

            Put_Line
              ("Wooded acres:" & Wooded_Acres'Img & " Lumberyarded acres:" &
                 Lumberyarded_Acres'Img & "=" &
                 Integer'Image (Wooded_Acres * Lumberyarded_Acres));
         end;
      end if;
      for Y in Main_Yard'Range (2) loop
         for X in Main_Yard'Range (1) loop
            declare
               Counter   : Natural := 0;
               Counter_2 : Natural := 0;
            begin
               case Main_Yard (X, Y).State is
                  when OPEN_GROUND =>
                     for DY in Y - 1 .. Y + 1 loop
                        for DX in X - 1 .. X + 1 loop
                           if In_Bounds (DX, DY, Main_Yard)
                             and then Main_Yard (DX, DY).State = TREES then
                              Counter := Counter + 1;
                           end if;
                        end loop;
                     end loop;
                     if Counter >= 3 then
                        Temp_Yard (X, Y).State := TREES;
                     else
                        Temp_Yard (X, Y).State := OPEN_GROUND;
                     end if;
                  when TREES =>
                     for DY in Y - 1 .. Y + 1 loop
                        for DX in X - 1 .. X + 1 loop
                           if In_Bounds (DX, DY, Main_Yard)
                             and then Main_Yard (DX, DY).State = LUMBERYARD
                           then
                              Counter := Counter + 1;
                           end if;
                        end loop;
                     end loop;
                     if Counter >= 3 then
                        Temp_Yard (X, Y).State := LUMBERYARD;
                     else
                        Temp_Yard (X, Y).State := TREES;
                     end if;
                  when LUMBERYARD =>
                     for DY in Y - 1 .. Y + 1 loop
                        for DX in X - 1 .. X + 1 loop
                           if In_Bounds (DX, DY, Main_Yard)
                             and then Main_Yard (DX, DY).State = TREES then
                              Counter := Counter + 1;
                           end if;
                           if In_Bounds (DX, DY, Main_Yard)
                             and then Main_Yard (DX, DY).State = LUMBERYARD
                           then
                              Counter_2 := Counter_2 + 1;
                           end if;
                        end loop;
                     end loop;
                     if Counter >= 1 and then Counter_2 >= 2 then
                        Temp_Yard (X, Y).State := LUMBERYARD;
                     else
                        Temp_Yard (X, Y).State := OPEN_GROUND;
                     end if;
                  when others =>
                     Put_Line ("Weird acre X=" & X'Img & " Y=" & Y'Img);
               end case;
            end;
         end loop;
      end loop;
      --Main_Yard := Temp_Yard;
      for Y in Main_Yard'Range (2) loop
         for X in Main_Yard'Range (1) loop
            Main_Yard (X, Y).State := Temp_Yard (X, Y).State;
         end loop;
      end loop;
      --Print_Yard(Main_Yard);
   end loop;
   declare
      Wooded_Acres       : Natural := 0;
      Lumberyarded_Acres : Natural := 0;
   begin
      for Y in Main_Yard'Range (2) loop
         for X in Main_Yard'Range (1) loop
            if Main_Yard (X, Y).State = TREES then
               Wooded_Acres := Wooded_Acres + 1;
            elsif Main_Yard (X, Y).State = LUMBERYARD then
               Lumberyarded_Acres := Lumberyarded_Acres + 1;
            end if;
         end loop;
      end loop;

      Put_Line
        ("Wooded acres:" & Wooded_Acres'Img & " Lumberyarded acres:" &
           Lumberyarded_Acres'Img);
   end;
end Main;
