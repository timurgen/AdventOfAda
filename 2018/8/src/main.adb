with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Unchecked_Deallocation;

procedure Main is

   type Node_Header is record
      Children : Natural;
      Metadata : Natural;
   end record;

   type Stack_Node;
   type Stack is access Stack_Node;
   type Stack_Node is record
      Element : Node_Header;
      Next    : Stack := null;
   end record;

   Stack_Empty_Error : exception;

   procedure Push (Item : Node_Header; Onto : in out Stack) is
      Temp : Stack := new Stack_Node;
   begin
      Temp.Element := Item;
      Temp.Next    := Onto;
      Onto         := Temp;
   end Push;

   procedure Pop (Item : out Node_Header; From : in out Stack) is
      procedure Free is new Ada.Unchecked_Deallocation (Stack_Node, Stack);
      Temp : Stack := From;
   begin
      if Temp = null then
         raise Stack_Empty_Error;
      end if;
      Item := Temp.Element;
      From := Temp.Next;
      Free (Temp);
   end Pop;
   function Is_Empty (Obj : Stack) return Boolean is (Obj = null);

   function Create return Stack is
   begin
      return (null);
   end Create;

   type Int_Array is array (Positive range <>) of Natural;

   function Split_String
     (S         : String;
      Delimeter : Character) return Int_Array
   is
      Result_Size : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) = Delimeter or else I = S'Last then
            Result_Size := Result_Size + 1;
         end if;
      end loop;
      declare
         Result_Arr    : Int_Array (1 .. Result_Size);
         Result_Cursor : Positive := Result_Arr'First;
         Cursor_Start  : Positive := S'First;
      begin
         for I in S'Range loop
            if I = S'Last or else S (I + 1) = Delimeter then
               Result_Arr (Result_Cursor) :=
                 Natural'Value (S (Cursor_Start .. I));
               Result_Cursor := Result_Cursor + 1;
               Cursor_Start  := I + 1;
            end if;
         end loop;
         return Result_Arr;
      end;
   end Split_String;

   Input_File : File_Type;
begin
   Open (Input_File, In_File, "test.txt");
   declare
      Line        : String    := Get_Line (Input_File);
      Data        : Int_Array := Split_String (Line, ' ');
      Stack_Obj   : Stack     := Create;
      Current_Header : Node_Header;
      Data_Idx: Natural := Data'First;
      Sum_Metadata : Natural := 0;
   begin
      Current_Header := (Data (Data_Idx), Data (Data_Idx+1));
      Data_Idx := Data_Idx + 2;
      Push (Current_Header, Stack_Obj);

      while not Is_Empty (Stack_Obj) loop
         Pop(Current_Header, Stack_Obj);
         if Current_Header.Children > 0 then
            Current_Header.Children := Current_Header.Children - 1;
            Push(Current_Header, Stack_Obj);
            declare
               Next_Child: Node_Header := (Data(Data_Idx),Data(Data_Idx +1));
            begin
               Push(Next_Child, Stack_Obj);
               Data_Idx := Data_Idx + 2;
            end;
         else
            for I in 1..Current_Header.Metadata loop
               --Put(Data(Data_Idx)'Img);
               Sum_Metadata := Sum_Metadata + Data(Data_Idx);
               Data_Idx := Data_Idx +1;
            end loop;
         end if;
      end loop;
      Put_Line("Sum of metadata" & Natural'Image(Sum_Metadata));
   end;
   Close (Input_File);
end Main;
