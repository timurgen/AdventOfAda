with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure Main is
   function Compare_String (Left, Right : String) return Boolean is (Left < Right);

   function Starts_With
     (S      : String;
      Needle : String) return Boolean is
     (S'Length >= Needle'Length and then S (S'First .. S'First + Needle'Length - 1) = Needle);

   package String_V is new Ada.Containers.Indefinite_Vectors (Index_Type => Positive, Element_Type => String);
   package String_V_Sorter is new String_V.Generic_Sorting ("<" => Compare_String);

   type Sleeping_Minutes_Arr is array (Natural range 0 .. 59) of Natural;

   type Guard_Record;
   type Guard_Record_Access is access all Guard_Record;

   type Guard_Record is record
      Id               : Natural              := 0;
      Sleeping_Minutes : Sleeping_Minutes_Arr := (others => 0);
      Next_Guard       : Guard_Record_Access  := null;
   end record;

   Root : Guard_Record;

   function Is_Guard_In_List (Id : Natural) return Boolean is
      Next_Guard : Guard_Record_Access := Root.Next_Guard;
   begin
      while Next_Guard /= null loop
         if Next_Guard.Id = Id then
            return True;
         end if;
         Next_Guard := Next_Guard.Next_Guard;
      end loop;
      return False;
   end Is_Guard_In_List;

   Bad_Guard : exception;

   function Get_Guard (Id : Natural) return Guard_Record_Access is
      Next_Guard : Guard_Record_Access := Root.Next_Guard;
   begin
      while Next_Guard /= null loop
         if Next_Guard.Id = Id then
            return Next_Guard;
         end if;
         Next_Guard := Next_Guard.Next_Guard;
      end loop;
      raise Bad_Guard;
   end Get_Guard;

   procedure Put_Guard (Id : Natural) is
      New_Guard : Guard_Record_Access := new Guard_Record;
   begin
      New_Guard.Id := Id;
      if Root.Next_Guard = null then
         Root.Next_Guard := New_Guard;
      else
         declare
            Last_Guard : Guard_Record_Access := Root.Next_Guard;
         begin
            loop
               if Last_Guard.Next_Guard = null then
                  Last_Guard.Next_Guard := New_Guard;
                  exit;
               else
                  Last_Guard := Last_Guard.Next_Guard;
               end if;
            end loop;
         end;
      end if;
   end Put_Guard;

   Bad_Sleep : exception;

   Raw_Input_Data_Vector : String_V.Vector;

   Input_File : File_Type;

   Most_significant_Minute   : Natural := 0;
   What_Minute               : Natural := 0;
   Max_Sleeping_Minutes      : Natural := 0;
   Guard_Id_With_Max_Minutes : Natural := 0;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      exit when End_Of_File (Input_File);
      declare
         Line : String := Get_Line (Input_File);
      begin
         Raw_Input_Data_Vector.Append (New_Item => Line);
      end;
   end loop;
   Close (File => Input_File);
   String_V_Sorter.Sort (Container => Raw_Input_Data_Vector);
   declare
      Last_Known_Guard_Id      : Natural;
      Last_Known_Sleep_Minute  : Natural;
      Last_Known_Wakeup_Minute : Natural;
   begin
      for I in Raw_Input_Data_Vector.First_Index .. Raw_Input_Data_Vector.Last_Index loop
         declare
            Line          : String              := Raw_Input_Data_Vector (I);
            Guard_Id      : Natural;
            Current_Guard : Guard_Record_Access := null;
         begin
            if Starts_With (Line (20 .. Line'Last), "Guard #") then
               Guard_Id            := Natural'Value (Line (27 .. Index (Line (27 .. Line'Last), " ")));
               Last_Known_Guard_Id := Guard_Id;
            elsif Starts_With (Line (20 .. Line'Last), "falls asleep") then
               Last_Known_Sleep_Minute := Natural'Value (Line (16 .. 17));
            else -- wake up
               Last_Known_Wakeup_Minute := Natural'Value (Line (16 .. 17));
               if Last_Known_Wakeup_Minute < Last_Known_Sleep_Minute then
                  raise Bad_Sleep;
               end if;
               if not Is_Guard_In_List (Last_Known_Guard_Id) then
                  Put_Guard (Last_Known_Guard_Id);
               end if;
               Current_Guard := Get_Guard (Last_Known_Guard_Id);
               for Minute in Last_Known_Sleep_Minute .. Last_Known_Wakeup_Minute - 1 loop
                  Current_Guard.Sleeping_Minutes (Minute) := Current_Guard.Sleeping_Minutes (Minute) + 1;
               end loop;
            end if;
         end;
      end loop;
      -- computing result
      declare
         Current_Guard                      : Guard_Record_Access := Root.Next_Guard;
         Last_Known_Sleeping_Minutes_Sum    : Natural             := 0;
         Last_Known_Most_Significant_Minute : Natural             := 0;
      begin
         while Current_Guard /= null loop
            Last_Known_Sleeping_Minutes_Sum    := 0;
            Last_Known_Most_Significant_Minute := 0;
            for I in Current_Guard.Sleeping_Minutes'Range loop
               if Last_Known_Most_Significant_Minute < Current_Guard.Sleeping_Minutes (I) then
                  Last_Known_Most_Significant_Minute := Current_Guard.Sleeping_Minutes (I);
                  What_Minute                        := I;
               end if;
               Last_Known_Sleeping_Minutes_Sum := Last_Known_Sleeping_Minutes_Sum + Current_Guard.Sleeping_Minutes (I);
            end loop;
            if Max_Sleeping_Minutes < Last_Known_Sleeping_Minutes_Sum then
               Max_Sleeping_Minutes      := Last_Known_Sleeping_Minutes_Sum;
               Guard_Id_With_Max_Minutes := Current_Guard.Id;
            end if;
            Put_Line
              (Current_Guard.Id'Img &
               Last_Known_Sleeping_Minutes_Sum'Img &
               What_Minute'Img &
               Last_Known_Most_Significant_Minute'Img);
            Current_Guard := Current_Guard.Next_Guard;
         end loop;

      end;
   end;
end Main;
