with Ada.Text_IO;

procedure Main is
   use Ada;
   Input_File      : Text_IO.File_Type;
   Start_Frequence : Integer := 0;
   type Operation_T is (ADD, SUBSTRACT, UNKNOWN);
   Unknon_Operation : exception;

   function Parse_Operation (S : String) return Operation_T is
      Sign : Character := S (S'First);
   begin
      if Sign = '+' then
         return ADD;
      elsif Sign = '-' then
         return SUBSTRACT;
      else
         return UNKNOWN;
      end if;
   end Parse_Operation;

   function Parse_Value (S : String) return Integer is
      Number_Str : String := S (S'First + 1 .. S'Last);
   begin
      return Integer'Value (Number_Str);
   end Parse_Value;

   --part 2 vars
   Already_Seen_Number : Integer := Start_Frequence;

   type Already_Seen_Index_Arr is
     array (Integer range -1_000_000 .. 100_000_000) of Boolean;
   type Already_Seen_Index_Arr_Access is access Already_Seen_Index_Arr;

   Already_Seen_Arr : Already_Seen_Index_Arr_Access := new Already_Seen_Index_Arr'(others => False);

   function Is_Already_Seen (I : Integer) return Boolean is
   begin
      return Already_Seen_Arr (I);
   end Is_Already_Seen;

   procedure Add_To_Already_Seen (I : Integer) is
   begin
      Already_Seen_Arr (I) := True;
   end Add_To_Already_Seen;

   procedure Register_Already_Seen_But_Only_First_Time (I : Integer) is
   begin
      if Already_Seen_Number = 0 then
         Already_Seen_Number := I;
      end if;

   end Register_Already_Seen_But_Only_First_Time;

begin
   loop
      exit when Already_Seen_Number /= 0;
      Text_IO.Open
        (File => Input_File,
         Mode => Text_IO.In_File,
         Name => "input.txt");
      loop
         exit when Text_IO.End_Of_File (Input_File);
         declare
            Line      : String      := Text_IO.Get_Line (Input_File);
            Operation : Operation_T := Parse_Operation (Line);
            Num       : Integer     := Parse_Value (Line);
         begin
            if Operation = ADD then
               Start_Frequence := Start_Frequence + Num;
            elsif Operation = SUBSTRACT then
               Start_Frequence := Start_Frequence - Num;
            else
               raise Unknon_Operation;
            end if;
            -- part 2
            if not Is_Already_Seen (Start_Frequence) then
               Add_To_Already_Seen (Start_Frequence);
            else
               Already_Seen_Number := Start_Frequence;
               exit;
            end if;

         end;
      end loop;
      Text_IO.Close(Input_File);
      Text_IO.Put_Line (Integer'Image (Start_Frequence));
      Text_IO.Put_Line (Integer'Image (Already_Seen_Number));
   end loop;

end Main;
