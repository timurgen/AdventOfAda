-- https://adventofcode.com/2015/day/2

pragma Initialize_Scalars;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.String_Split;

procedure Main is

   type Square_Feet is new Integer;

   Wrapping_Paper_Needed : Square_Feet := 0;
   Ribbon_Needed: integer := 0;

   type Present_Dimensions is record
      L : Positive;
      W : Positive;
      H : Positive;
   end record;

   function Extract_Dimension (S : String) return Present_Dimensions is
      Dim_Delimeter  : Character := 'x';
      Cursor         : Integer   := 1;
      Last_Dim_Start : Integer   := 1;
      L              : Positive;
      W              : Positive;
      H              : Positive;
   begin
      for I in 1 .. S'Length loop
         if S (I) = Dim_Delimeter or I = S'Last then
            if not L'Valid then
               L              := Positive'Value (S (Last_Dim_Start .. Cursor - 1));
               Last_Dim_Start := Cursor + 1;
            elsif not W'Valid then
               W              := Positive'Value (S (Last_Dim_Start .. Cursor - 1));
               Last_Dim_Start := Cursor + 1;
            elsif not H'Valid then
               H              := Positive'Value (S (Last_Dim_Start .. Cursor));
               Last_Dim_Start := Cursor + 1;
            else
               null; -- something went wrong
            end if;

         end if;
         Cursor := Cursor + 1;
      end loop;

      return (L, W, H);
   end Extract_Dimension;

   function "*" (Left : Integer; Right : Positive) return Square_Feet is (Square_Feet (Left) * Square_Feet (Right)) with
      Inline;

   function Compute_Paper_Needed (Item : Present_Dimensions) return Square_Feet is
      LW2, WH2, HL2, Min_Size : Square_Feet;
   begin
      LW2      := 2 * Item.L * Item.W;
      WH2      := 2 * Item.W * Item.H;
      HL2      := 2 * Item.H * Item.L;
      Min_Size := Square_Feet'Min (Square_Feet'Min (LW2, WH2), Square_Feet'Min (WH2, HL2)) / 2;
      return LW2 + WH2 + HL2 + Min_Size;
   end Compute_Paper_Needed;

   function Compute_Ribbon_Needed(Item: Present_Dimensions) return Positive is
      Tail_Length, Shortest_Perimeter_Length: Positive;
      LWP, WHP, HLP: Positive;
   begin
      LWP := Item.L * 2 +  Item.W * 2;
      WHP := Item.W *2 + Item.H *2;
      HLP := Item.H *2 + Item.L *2;
      Tail_Length := Item.L * Item.W * Item.H;
      Shortest_Perimeter_Length := Positive'Min(Positive'Min(LWP, WHP), Positive'Min(WHP, HLP));
      return Tail_Length + Shortest_Perimeter_Length;
   end Compute_Ribbon_Needed;


   procedure Read_File_Line_By_Line is
      Input : File_Type;
   begin
      Open (File => Input, Mode => In_File, Name => "input.txt");
      loop
         declare
            Line                    : String := Get_Line (Input);
            Current_Present         : Present_Dimensions;
            Current_Present_Surface : Square_Feet;
            Current_Present_Ribbon  : Positive;
         begin
            Current_Present         := Extract_Dimension (Line);
            Current_Present_Surface := Compute_Paper_Needed (Current_Present);
            Current_Present_Ribbon := Compute_Ribbon_Needed(Current_Present);
            Wrapping_Paper_Needed   := Wrapping_Paper_Needed + Current_Present_Surface;
            Ribbon_Needed := Ribbon_Needed + Current_Present_Ribbon;
         end;
      end loop;
   exception
      when End_Error =>
         if Is_Open (Input) then
            Close (Input);
         end if;
   end Read_File_Line_By_Line;

begin
   Read_File_Line_By_Line;
   Put_Line (Square_Feet'Image (Wrapping_Paper_Needed));
   Put_Line(Positive'Image(Ribbon_Needed));
end Main;
