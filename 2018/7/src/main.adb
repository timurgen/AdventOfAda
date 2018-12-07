with Ada.Text_IO; use Ada.Text_IO;
with Digraph;

procedure Main is

   type Letter_Node;
   type Letter_Node_Access is access all Letter_Node;
   type Letter_Node_Array is array (1..15) of Letter_Node_Access;

   type Letter_Node is record
      Value : Character;
      Next_Nodes:Letter_Node_Array := (others => null);
   end record;

   procedure Parse_Values (S : in String; A, B : out Character) is
   begin
      A := S (S'First + 5);
      B := S (S'First + 36);
   end Parse_Values;


   Root_Letter: Letter_Node;
   Input_File : File_Type;
begin
   Open (Input_File, In_File, "test.txt");
   loop
      declare
         Line : String := Get_Line (Input_File);
         A, B : Character;
      begin
         Parse_Values (Line, A, B);
         Put_Line (A & " " & B);
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (Input_File);
end Main;
