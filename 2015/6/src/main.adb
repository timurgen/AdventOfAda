with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Light is record
      Is_Turned_On : Boolean := False;
   end record;

   Light_Grid : array (0 .. 999, 0 .. 999) of Light;

   Input_File : File_Type;

   Command_Turn_Off : constant String := "turn off";
   Command_Turn_On  : constant String := "turn on";
   Command_Toggle   : constant String := "toggle";

   Number_On : Integer := 0;

   function Starts_With (S : String; Needle : String) return Boolean is
     (S'Length >= Needle'Length and then S (S'First .. S'First + Needle'Length - 1) = Needle);

   procedure Turn_On (X1 : Integer; Y1 : Integer; X2 : Integer; Y2 : Integer) is
   begin
      for I in X1 .. X2 loop
         for J in Y1 .. Y2 loop
            Light_Grid (I, J).Is_Turned_On := True;
         end loop;
      end loop;
   end Turn_On;

   procedure Turn_Off (X1 : Integer; Y1 : Integer; X2 : Integer; Y2 : Integer) is
   begin
      for I in X1 .. X2 loop
         for J in Y1 .. Y2 loop
            Light_Grid (I, J).Is_Turned_On := False;
         end loop;
      end loop;
   end Turn_Off;

   procedure Toggle (X1 : Integer; Y1 : Integer; X2 : Integer; Y2 : Integer) is
   begin
      for I in X1 .. X2 loop
         for J in Y1 .. Y2 loop
            Light_Grid (I, J).Is_Turned_On := not Light_Grid (I, J).Is_Turned_On;
         end loop;
      end loop;
   end Toggle;

   procedure Extract_Coordinates (X1 : out Integer; X2 : out Integer; Y1 : out Integer; Y2 : out Integer; S : in String)
   is
      function Is_Numeric (C : String) return Boolean is
         Dummy : Integer;
      begin
         Dummy := Integer'Value (C);
         return True;
      exception
         when others =>
            return False;
      end Is_Numeric;

      First  : Natural := S'First;
      Second : Natural := S'First;

      Coordinates   : array (1 .. 4) of Integer;
      Current_Index : Integer := 1;
   begin
      loop
         -- start of digit
         while not Is_Numeric ("" & S (First)) loop
            First := First + 1;
         end loop;
         -- end of digit
         Second := First;
         while Second <= S'Last and then Is_Numeric ("" & S (Second)) loop
            Second := Second + 1;
         end loop;

         Coordinates (Current_Index) := Integer'Value (S (First .. Second - 1));
         Current_Index               := Current_Index + 1;
         First                       := Second;

         exit when First > S'Last;
      end loop;
      X1 := Coordinates (1);
      Y1 := Coordinates (2);
      X2 := Coordinates (3);
      Y2 := Coordinates (4);
   end Extract_Coordinates;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => In_File, Name => "input.txt");

   loop

      declare
         Line : String := Get_Line (Input_File);
         X1   : Integer;
         X2   : Integer;
         Y1   : Integer;
         Y2   : Integer;
      begin
         Extract_Coordinates (X1 => X1, X2 => X2, Y1 => Y1, Y2 => Y2, S => Line);
         if Starts_With (S => Line, Needle => Command_Turn_Off) then
            Put_Line ("Turning off " & X1'Image & ", " & Y1'Image & " through " & X2'Image & ", " & Y2'Image);
            Turn_Off (X1 => X1, Y1 => Y1, X2 => X2, Y2 => Y2);
         elsif Starts_With (S => Line, Needle => Command_Turn_On) then
            Put_Line ("Turning on " & X1'Image & ", " & Y1'Image & " through " & X2'Image & ", " & Y2'Image);
            Turn_On (X1 => X1, Y1 => Y1, X2 => X2, Y2 => Y2);
         elsif Starts_With (S => Line, Needle => Command_Toggle) then
            Put_Line ("Toggle " & X1'Image & ", " & Y1'Image & " through " & X2'Image & ", " & Y2'Image);
            Toggle (X1 => X1, Y1 => Y1, X2 => X2, Y2 => Y2);
         else
            Put_Line ("Wat?");
         end if;

      end;
   end loop;
exception
   when End_Error =>
      if Is_Open (Input_File) then
         for I in Light_Grid'Range (1) loop
            for J in Light_Grid'Range (2) loop
               if Light_Grid (I, J).Is_Turned_On then
                  Number_On := Number_On + 1;
               end if;
            end loop;
         end loop;
         Put_Line (Number_On'Img);
         Close (Input_File);
      end if;
end Main;
