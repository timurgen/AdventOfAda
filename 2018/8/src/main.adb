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
      Last_Header : Node_Header;
      Idx: Natural := 2;
   begin
      Last_Header := (Data (Data'First), Data (Data'First + 1));
      Push (Last_Header, Stack_Obj);
      while not Is_Empty (Stack_Obj) loop
         if Last_Header.Children > 0 then
            Pop(Last_Header,Stack_Obj);
            Last_Header.Children := Last_Header.Children -1;
            Push(Last_Header, Stack_Obj);
            Push((Data(Idx+1), Data(Idx+2)), Stack_Obj);
            Idx := Idx+2;
         elsif Last_Header.Metadata > 0 then
            Pop(Last_Header, Stack_Obj);
            Last_Header.Metadata := Last_Header.Metadata - 1;
            Put(Data(Idx)'Img);
            Push(Last_Header, Stack_Obj);
            Idx := Idx + 1;
         else
            Pop(Last_Header, Stack_Obj);
         end if;

      end loop;

   end;
   Close (Input_File);
end Main;
