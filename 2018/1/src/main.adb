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

begin
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
      end;
   end loop;
   Text_IO.Put_Line (Integer'Image (Start_Frequence));
end Main;
