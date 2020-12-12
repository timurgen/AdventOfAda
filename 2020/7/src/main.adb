pragma Ada_2020;

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

procedure Main is
   package String_Vectors is new Indefinite_Vectors (Positive, String);
   use String_Vectors;

   function Is_Equal_Strings (Left, Right : Unbounded_String) return Boolean is
   begin
      return To_String (Left) = To_String (Right);
   end Is_Equal_Strings;

   package Bag_Content_Map is new Hashed_Maps
     (Key_Type => Unbounded_String, Element_Type => Integer,
      Hash => Ada.Strings.Unbounded.Hash, Equivalent_Keys => Is_Equal_Strings);

   type Bag_Rule is record
      Bag_Name    : Unbounded_String;
      Bag_Content : Bag_Content_Map.Map;
   end record;

   package Bag_Vectors is new Indefinite_Vectors (Positive, Bag_Rule);

   ---
   --- Split String S into Vector by Separator character
   ---
   function Split_String (S : String; Separator : Character) return Vector is
      Result : Vector;
      First  : Positive := S'First;
      Last   : Positive := S'First;
   begin
      loop
         -- end of string, put as last substring
         if Last = S'Last then
            Result.Append (New_Item => S (First .. Last));
            exit;
         end if;

         -- iterating to next separator
         if S (Last) /= Separator then
            Last := Last + 1;
            goto Continue;
         end if;

         -- nothing but separator
         if Last = First then
            Result.Append (New_Item => "");
            -- actual substring
         else
            Result.Append (New_Item => S (First .. Last - 1));
            First := Last + 1;
         end if;
         Last := Last + 1;

         <<Continue>>
      end loop;
      return Result;
   end Split_String;

   function Parse_Bag_Rule (S : String) return Bag_Rule is
      Split_By  : constant String := " contain ";
      Empty_Bag : constant String := "no other bags.";
      Cursor    : Positive        := S'First;
      Result    : Bag_Rule;
      Parts     : Vector;
   begin
      loop
         exit when
           (for all I in Split_By'Range => Split_By (I) = S (Cursor + I - 1))
           or else Cursor = S'Last;
         Cursor := Cursor + 1;
      end loop;

      Result.Bag_Name := To_Unbounded_String (S (S'First .. Cursor - 1));
      Parts           :=
        Split_String
          (S => S (Cursor + Split_By'Length .. S'Last), Separator => ',');

      if Parts.Length = 1 and then Parts.Element (1) = Empty_Bag then
         return Result;
      end if;

      for Part of Parts loop
         declare
            S : String := Trim (Source => Part, Side => Ada.Strings.Both);

            Amount : Positive         := Positive'Value (S (1) & "");
            Name   : Unbounded_String := To_Unbounded_String (S (3 .. S'Last));
         begin
            if Length (Name) > 1 and then Element (Name, Length (Name)) = '.'
            then
               Name := Unbounded_Slice (Name, 1, Length (Name) - 1);
            end if;
            if Amount = 1 then
               Append (Name, To_Unbounded_String ("s"));
            end if;
            Result.Bag_Content.Insert (Name, Amount);
         end;
      end loop;
      return Result;
   end Parse_Bag_Rule;

   function Find_By_Name
     (N : Unbounded_String; BV : Bag_Vectors.Vector) return Bag_Rule
   is
      Result : Bag_Rule;
   begin
      for Item of BV loop
         if Item.Bag_Name = N then
            return Item;
         end if;
      end loop;
      return Result;
   end Find_By_Name;

   function May_Contain_At_Least_One_Shiny_Gold_Bag
     (Bag : Bag_Rule; List : Bag_Vectors.Vector) return Boolean
   is
   begin

      return
        (for some I in Bag.Bag_Content.Iterate =>
           Bag_Content_Map.Key (I) = To_Unbounded_String ("shiny gold bags")
           or else May_Contain_At_Least_One_Shiny_Gold_Bag
             (Bag  => Find_By_Name (Bag_Content_Map.Key (I), List),
              List => List));
   end May_Contain_At_Least_One_Shiny_Gold_Bag;

   procedure Count_Bags
     (Bag    :        Bag_Rule; List : Bag_Vectors.Vector;
      Result : in out Long_Long_Integer)
   is
   begin
      for B in Bag.Bag_Content.Iterate loop
         Result := Result + Long_Long_Integer (Bag_Content_Map.Element (B));
         for I in 1 .. Bag_Content_Map.Element (B) loop
            Count_Bags
              (Bag  => Find_By_Name (Bag_Content_Map.Key (B), List),
               List => List, Result => Result);
         end loop;
      end loop;
   end Count_Bags;

   Input_File    : File_Type;
   Bag_Rule_List : Bag_Vectors.Vector;
   -- part 1
   Counter : Natural := 0;
   --part 2
   Sum_Bags : Long_Long_Integer := 0;
begin
   -- prepare
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Line : String   := Get_Line (Input_File);
         Bag  : Bag_Rule := Parse_Bag_Rule (Line);
      begin
         Bag_Rule_List.Append (New_Item => Bag);
         exit when End_Of_File (File => Input_File);
      end;
   end loop;
   Close (File => Input_File);

   -- part 1
   for Bag_Rule_Obj of Bag_Rule_List loop
      if May_Contain_At_Least_One_Shiny_Gold_Bag (Bag_Rule_Obj, Bag_Rule_List)
      then
         Counter := Counter + 1;
      end if;
   end loop;
   Put_Line ("part 1 " & Counter'Image);
   --part 2

   Count_Bags
     (Bag =>
        Find_By_Name (To_Unbounded_String ("shiny gold bags"), Bag_Rule_List),
      List => Bag_Rule_List, Result => Sum_Bags);

   Put_Line ("part 2 " & Sum_Bags'Image);
end Main;
