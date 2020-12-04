with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   package String_Vectors is new Indefinite_Vectors (Positive, String);
   use String_Vectors;

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

   ---
   --- Passport datastructure
   ---
   type Passport is record
      Byr : Unbounded_String;
      Iyr : Unbounded_String;
      Eyr : Unbounded_String;
      Hgt : Unbounded_String;
      Hcl : Unbounded_String;
      Ecl : Unbounded_String;
      Pid : Unbounded_String;
      Cid : Unbounded_String;
   end record;
   type Passport_Ptr is access all Passport;

   ---
   --- Function to check if given String S is empty
   ---
   function Is_Blank_Line (S : String) return Boolean is
   begin
      return S'Length = 0;
   end Is_Blank_Line;

   procedure Parse_Line_Into_Pass (S : in String; P : in out Passport_Ptr) is
      type Pass_Key is (byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
      Parts : Vector := Split_String (S, ' ');
   begin
      for Part of Parts loop
         declare
            Part_KV : Vector := Split_String (Part, ':');
         begin
            case Pass_Key'Value (Part_KV.Element (1)) is
               when byr =>
                  P.all.Byr := To_Unbounded_String (Part_KV.Element (2));
               when iyr =>
                  P.all.Iyr := To_Unbounded_String (Part_KV.Element (2));
               when eyr =>
                  P.all.Eyr := To_Unbounded_String (Part_KV.Element (2));
               when hgt =>
                  P.all.Hgt := To_Unbounded_String (Part_KV.Element (2));
               when hcl =>
                  P.all.Hcl := To_Unbounded_String (Part_KV.Element (2));
               when ecl =>
                  P.all.Ecl := To_Unbounded_String (Part_KV.Element (2));
               when pid =>
                  P.all.Pid := To_Unbounded_String (Part_KV.Element (2));
               when cid =>
                  P.all.Cid := To_Unbounded_String (Part_KV.Element (2));

            end case;
         end;
      end loop;
   end Parse_Line_Into_Pass;

   function Is_Valid_Pass (P : Passport_Ptr) return Boolean is
   begin
      return
        P.all.Byr /= "" and P.all.Iyr /= "" and P.all.Eyr /= "" and
        P.all.Hgt /= "" and P.all.Hcl /= "" and P.all.Ecl /= "" and
        P.all.Pid /= "";
   end Is_Valid_Pass;

   function Is_Valid_Pass2 (P : Passport_Ptr) return Boolean is
      function Is_Valid_Hgt (S : Unbounded_String) return Boolean is
         S2    : String := To_String (S);
         Value : Integer;
         Unit  : String (1 .. 2);
      begin
         if S2 = "" then
            return False;
         end if;

         Unit := S2 (S2'Last - 1 .. S2'Last);

         if Unit = "cm" then
            Value := Integer'Value (S2 (S2'First .. S2'Last - 2));
            return Value >= 150 and Value <= 193;
         end if;

         if Unit = "in" then
            Value := Integer'Value (S2 (S2'First .. S2'Last - 2));
            return Value >= 59 and Value <= 76;
         end if;
         return False;
      end Is_Valid_Hgt;

      function Is_Valid_Hcl (S : Unbounded_String) return Boolean is
         S2 : String := To_String (S);
      begin
         return
           S2'Length = 7
           and then
           (S2 (1) = '#' and
            (for all I in S2'First + 1 .. S2'Last =>
               S2 (I) in '0' .. '9' | 'a' .. 'f'));
      end Is_Valid_Hcl;

      function Is_Valid_Ecl (S : Unbounded_String) return Boolean is
         S2 : String := To_String (S);
      begin
         return
           S2'Length = 3
           and then S2 in "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" |
               "oth";
      end Is_Valid_Ecl;

      function Is_Valid_Pid (S : Unbounded_String) return Boolean is
         S2 : String := To_String (S);
      begin
         return
           S2'Length = 9
           and then (for all I in S2'Range => S2 (I) in '0' .. '9');
      end Is_Valid_Pid;
      Byr : Positive;
      Iyr : Positive;
      Eyr : Positive;
   begin
      if P.all.Byr /= "" then
         Byr := Positive'Value (To_String (P.all.Byr));
      end if;
      if P.all.Iyr /= "" then
         Iyr := Positive'Value (To_String (P.all.Iyr));
      end if;

      if P.all.Eyr /= "" then
         Eyr := Positive'Value (To_String (P.all.Eyr));
      end if;

      return
        Is_Valid_Pass (P) and (Byr >= 1_920 and Byr <= 2_002) and
        (Iyr >= 2_010 and Iyr <= 2_020) and (Eyr >= 2_020 and Eyr <= 2_030) and
        Is_Valid_Hgt (P.all.Hgt) and Is_Valid_Hcl (P.all.Hcl) and
        Is_Valid_Ecl (P.all.Ecl) and Is_Valid_Pid (P.all.Pid);
   end Is_Valid_Pass2;

   procedure Reset_Passport (P : in out Passport_Ptr) is
   begin
      P.all.Byr := To_Unbounded_String ("");
      P.all.Iyr := To_Unbounded_String ("");
      P.all.Eyr := To_Unbounded_String ("");
      P.all.Hgt := To_Unbounded_String ("");
      P.all.Hcl := To_Unbounded_String ("");
      P.all.Ecl := To_Unbounded_String ("");
      P.all.Pid := To_Unbounded_String ("");
      P.all.Cid := To_Unbounded_String ("");
   end Reset_Passport;

   Input_File : File_Type;

   -- part 1
   Current_Pass     : aliased Passport;
   Current_Pass_Ptr : Passport_Ptr := Current_Pass'Access;
   Total_Passports  : Natural      := 0;
   Valid_Passports1 : Natural      := 0;
   Valid_Passports2 : Natural      := 0;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Line : String := Get_Line (File => Input_File);
      begin
         if Is_Blank_Line (Line) or End_Of_File (Input_File) then
            if Is_Valid_Pass (Current_Pass_Ptr) then
               Valid_Passports1 := Valid_Passports1 + 1;
            end if;
            if Is_Valid_Pass2 (Current_Pass_Ptr) then
               Valid_Passports2 := Valid_Passports2 + 1;
            end if;
            Total_Passports := Total_Passports + 1;
            Reset_Passport (Current_Pass_Ptr);

            if End_Of_File (Input_File) then
               exit;
            end if;
         else
            Parse_Line_Into_Pass (S => Line, P => Current_Pass_Ptr);
         end if;
      end;
   end loop;
   Close (File => Input_File);
   Put_Line (Valid_Passports1'Image & " of" & Total_Passports'Image);
   Put_Line (Valid_Passports2'Image & " of" & Total_Passports'Image);
end Main;
