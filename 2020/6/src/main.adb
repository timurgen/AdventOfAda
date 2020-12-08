pragma Ada_2020;

with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Main is

   type Group_Answers is array (Character range 'a' .. 'z') of Natural with
      Default_Component_Value => 0;
   type Group is record
      Answers    : Group_Answers;
      Group_Size : Natural;
   end record;

   package Group_List_P is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => Group);

   Input_File    : File_Type;
   Cursor        : Positive := 1;
   Groups        : Group_List_P.Vector;
   Current_Group : Group;
   Group_Size    : Natural  := 0;
   Sum           : Integer  := 0;
   Sum2           : Integer  := 0;
begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");
   loop
      declare
         Line : String := Get_Line (Input_File);
      begin
         if Line = "" or else End_Of_File (Input_File) then
            Current_Group.Group_Size := Group_Size;
            Groups.Append (Current_Group);
            Group_Size            := 0;
            Current_Group.Answers := (others => 0);
            goto Continue;
         else
            Group_Size := Group_Size + 1;
            for Ch of Line loop
               Current_Group.Answers (Ch) := Current_Group.Answers (Ch) + 1;
            end loop;
         end if;

         <<Continue>>
         exit when End_Of_File (Input_File);
      end;
   end loop;
   Close (File => Input_File);

   -- part 1
   for Gr of Groups loop
      for I of Gr.Answers loop
         Sum := Sum + (if I > 0 then 1 else 0);
      end loop;
   end loop;
   Put_Line ("part 1 " & Sum'Image);
   -- part 2
   for Gr of Groups loop
      for I of Gr.Answers loop
         Sum2 := Sum2 + (if I = Gr.Group_Size then 1 else 0);
      end loop;
   end loop;
   Put_Line ("part 2 " & Sum2'Image);
end Main;
