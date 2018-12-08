with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Unchecked_Deallocation;

procedure Main is
   type Int_Array is array (Positive range <>) of Natural;
   type Node_Header;
   type Node_Header_Ptr is access all Node_Header;

   package Values_V_P is new Indefinite_Vectors (Positive, Natural);
   package Children_V_P is new Indefinite_Vectors (Positive, Node_Header_Ptr);

   type Node_Header is record
      Children      : Natural;
      Metadata      : Natural;
      Values        : Values_V_P.Vector;
      Children_List : Children_V_P.Vector;
   end record;

   type Stack_Node;
   type Stack is access Stack_Node;
   type Stack_Node is record
      Element : Node_Header_Ptr;
      Next    : Stack := null;
   end record;

   Stack_Empty_Error : exception;

   procedure Push (Item : Node_Header_Ptr; Onto : in out Stack) is
      Temp : Stack := new Stack_Node;
   begin
      Temp.Element := Item;
      Temp.Next    := Onto;
      Onto         := Temp;
   end Push;

   procedure Pop (Item : out Node_Header_Ptr; From : in out Stack) is
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

   function Split_String (S : String; Delimeter : Character) return Int_Array
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

   function Recursive_Part_2 (Node : Node_Header_Ptr) return Natural is
      Sum : Natural := 0;
   begin
      if Node.Children_List.Length = 0 then
         for I in Node.Values.First_Index .. Node.Values.Last_Index loop
            Sum := Sum + Node.Values (I);
         end loop;
         return Sum;
      else
         for I in Node.Values.First_Index .. Node.Values.Last_Index loop
            declare
               Value              : Natural := Node.Values (I);
               Current_Child_Node : Node_Header_Ptr;
            begin
               if Value in
                 Node.Children_List.First_Index ..
                   Node.Children_List.Last_Index
               then
                  Current_Child_Node := Node.Children_List.Element (Value);
                  Sum := Sum + Recursive_Part_2 (Current_Child_Node);
               end if;
            end;
         end loop;
         return Sum;
      end if;
   end Recursive_Part_2;

   Input_File : File_Type;
begin
   Open (Input_File, In_File, "input.txt");
   declare
      Line           : String    := Get_Line (Input_File);
      Data           : Int_Array := Split_String (Line, ' ');
      Stack_Obj      : Stack     := Create;
      Data_Idx       : Natural   := Data'First;
      Sum_Metadata   : Natural   := 0;
      Current_Header : Node_Header_Ptr;
   begin
      Current_Header          := new Node_Header;
      Current_Header.Children := Data (Data_Idx);
      Current_Header.Metadata := Data (Data_Idx + 1);

      Data_Idx := Data_Idx + 2;
      Push (Current_Header, Stack_Obj);

      while not Is_Empty (Stack_Obj) loop
         Pop (Current_Header, Stack_Obj);
         if Current_Header.Children > 0 then
            Current_Header.Children := Current_Header.Children - 1;
            declare
               Next_Child : Node_Header_Ptr;
            begin
               Next_Child          := new Node_Header;
               Next_Child.Children := Data (Data_Idx);
               Next_Child.Metadata := Data (Data_Idx + 1);
               Current_Header.Children_List.Append (Next_Child);
               Push (Current_Header, Stack_Obj);
               Push (Next_Child, Stack_Obj);
               Data_Idx := Data_Idx + 2;
            end;
         else
            for I in 1 .. Current_Header.Metadata loop
               --Put(Data(Data_Idx)'Img);
               Current_Header.Values.Append (Data (Data_Idx));
               Sum_Metadata := Sum_Metadata + Data (Data_Idx);
               Data_Idx     := Data_Idx + 1;
            end loop;
         end if;
      end loop;
      Put_Line ("Part 1:" & Natural'Image (Sum_Metadata));
      Put_Line ("Part 2:" & Recursive_Part_2 (Current_Header)'Img);
   end;
   Close (Input_File);
end Main;
