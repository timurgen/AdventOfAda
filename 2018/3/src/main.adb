with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Vectors;

procedure Main is

   type Coordinate is record
      X : Natural;
      Y : Natural;
   end record;

   type Dimension is record
      Width  : Positive;
      Height : Positive;
   end record;

   type Claim is record
      ID     : Positive;
      Coords : Coordinate;
      Dims   : Dimension;
   end record;

   Bad_Claim : exception;

   package Claim_List_P is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Claim);

   function Parse_Claim (S : String) return Claim is
      function Parse_Id (S : String) return Positive is
         Cursor : Integer := S'First;
      begin
         if S (S'First) /= '#' then
            raise Bad_Claim;
         end if;
         for I in S'Range loop
            exit when S (I) = ' ';
            Cursor := Cursor + 1;
         end loop;
         return Positive'Value (S (S'First + 1 .. Cursor - 1));
      end Parse_Id;

      function Parse_XY (S : String) return Coordinate is
         Cursor     : Integer := S'First;
         Cursor_End : Integer := Cursor;
         X          : Natural;
         Y          : Natural;
      begin
         for I in S'Range loop
            exit when S (I) = '@';
            Cursor := Cursor + 1;
         end loop;
         Cursor := Cursor + 2; --space after @

         for I in Cursor .. S'Last loop
            exit when S (I) = ',';
            Cursor_End := I;
         end loop;

         X := Integer'Value (S (Cursor .. Cursor_End));

         Cursor := Cursor_End + 2;
         for I in Cursor .. S'Last loop
            exit when S (I) = ':';
            Cursor_End := I;
         end loop;

         Y := Integer'Value (S (Cursor .. Cursor_End));
         return (X, Y);
      end Parse_XY;

      function Parse_WH (S : String) return Dimension is
         Cursor     : Integer := S'First;
         Cursor_End : Integer := S'First;
         W          : Positive;
         H          : Positive;
      begin
         for I in S'Range loop
            exit when S (I) = ':';
            Cursor := Cursor + 1;
         end loop;
         Cursor := Cursor + 2; -- ': '

         Cursor_End := Cursor;

         for I in Cursor .. S'Last loop
            exit when S (I) = 'x';
            Cursor_End := I;
         end loop;

         W      := Positive'Value (S (Cursor .. Cursor_End));
         Cursor := Cursor_End + 2;
         H      := Positive'Value (S (Cursor .. S'Last));

         return (W, H);
      end Parse_WH;

      Id     : Positive   := Parse_Id (S);
      Coords : Coordinate := Parse_XY (S);
      Dims   : Dimension  := Parse_WH (S);
   begin
      return (Id, Coords, Dims);
   end Parse_Claim;

   Input_File : File_Type;
   Claims     : Claim_List_P.Vector;
   X_Size     : Positive := 1;
   Y_Size     : Positive := 1;
   Counter    : Natural  := 0;

   -- part 2 function
   function Do_Overlap (Left, Right : Claim) return Boolean is
   begin
      if Left.Coords.X > (Right.Coords.X + Right.Dims.Width - 1)
        or else Right.Coords.X > (Left.Coords.X + Left.Dims.Width - 1) then
         return False;
      end if;
      if Left.Coords.Y > (Right.Coords.Y + Right.Dims.Height - 1)
        or else Right.Coords.Y > (Left.Coords.Y + Left.Dims.Height - 1) then
         return False;
      end if;
      return True;
   end Do_Overlap;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop

      exit when End_Of_File (Input_File);
      declare
         Line          : String := Get_Line (Input_File);
         Current_Claim : Claim  := Parse_Claim (Line);
      begin
         Claims.Append (Current_Claim);
      end;
   end loop;
   Close (Input_File);

   --find out how big area we need
   for I in Claims.First_Index .. Claims.Last_Index loop
      declare
         Current_Claim : Claim    := Claims (I);
         Current_X     : Positive :=
           Current_Claim.Coords.X + Current_Claim.Dims.Width;
         Current_Y : Positive :=
           Current_Claim.Coords.Y + Current_Claim.Dims.Height;
      begin
         if Current_X > X_Size then
            X_Size := Current_X;
         end if;
         if Current_Y > Y_Size then
            Y_Size := Current_Y;
         end if;

      end;
   end loop;
   -- declare claim area and calculate number of overlapped cells
   declare
      Claim_Area : array (0 .. X_Size, 0 .. Y_Size) of Natural :=
        (others => (others => 0));
   begin
      for I in Claims.First_Index .. Claims.Last_Index loop
         declare
            Current_Claim : Claim      := Claims (I);
            Coords        : Coordinate := Current_Claim.Coords;
            Dims          : Dimension  := Current_Claim.Dims;
         begin
            for X in Coords.X .. Coords.X + Dims.Width - 1 loop
               for Y in Coords.Y .. Coords.Y + Dims.Height - 1 loop
                  Claim_Area (X, Y) := Claim_Area (X, Y) + 1;
               end loop;
            end loop;
         end;
      end loop;

      for X in Claim_Area'Range loop
         for Y in Claim_Area'Range (1) loop
            if Claim_Area (X, Y) = 0 then
               --Put(".");
               null;
            elsif Claim_Area (X, Y) = 1 then
               --Put("#");
               null;
            else
               --Put("X");
               Counter := Counter + 1;
            end if;
         end loop;
         -- New_Line;
      end loop;

   end;
   -- output part 1
   Put_Line (Counter'Img);
   Put_Line ("part two");
   --part 2
   for I in Claims.First_Index .. Claims.Last_Index loop
      declare
         IsXOverlaps : Boolean := False;
      begin
         IsXOverlaps := False;
         for J in Claims.First_Index .. Claims.Last_Index loop
            declare
               ClaimX : Claim := Claims (I);
               ClaimY : Claim := Claims (J);

            begin
               if ClaimX.ID /= ClaimY.ID and then Do_Overlap (ClaimX, ClaimY)
               then
                  IsXOverlaps := True;
               end if;

            end;
         end loop;
         if not IsXOverlaps then
            Put_Line ("Hurra" & Claims (I).ID'Img);
         end if;
      end;
   end loop;

end Main;
