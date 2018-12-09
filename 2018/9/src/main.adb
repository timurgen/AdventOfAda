with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
procedure Main is
   type Players_Number_T is range 1 .. Integer'Last;
   type Marble_Number_T is range 0 .. Integer'Last;

   type Marble_Node;
   type Marble_Node_Ptr is access Marble_Node;

   type Marble_Node is record
      Value    : Marble_Number_T;
      Previous : Marble_Node_Ptr;
      Next     : Marble_Node_Ptr;
   end record;

   Players_In_Game : Players_Number_T := 410;
   type Players_Arr is array (1 .. Players_In_Game) of Long_Long_Integer;

   Player_Score_Table : Players_Arr := (others => 0);

   Marbles_Max_Worth : Marble_Number_T := 7205900;

   Current_Marble : Marble_Number_T := 0;
   Next_Marble    : Marble_Number_T := 1;
   Marble_Circle  : Marble_Node_Ptr := new Marble_Node;

   Current_Player : Players_Number_T := 1;
begin
   Marble_Circle.Value    := 0;
   Marble_Circle.Previous := Marble_Circle;
   Marble_Circle.Next     := Marble_Circle;

   for Marble_Nr in 1 .. Marbles_Max_Worth loop
      declare
      begin
         if Marble_Nr mod 23 = 0 then
            Marble_Circle :=
              Marble_Circle.Previous.Previous.Previous.Previous.Previous
                .Previous
                .Previous;
            Player_Score_Table (Current_Player) :=
              Player_Score_Table (Current_Player) +
              Long_Long_Integer (Marble_Nr + Marble_Circle.Value);
            Marble_Circle               := Marble_Circle.Next;
            Marble_Circle.Previous      := Marble_Circle.Previous.Previous;
            Marble_Circle.Previous.Next := Marble_Circle;

         else
            Marble_Circle :=
              new Marble_Node'
                (Value    => Marble_Nr,
                 Previous => Marble_Circle.Next,
                 Next     => Marble_Circle.Next.Next);
            Marble_Circle.Next.Previous := Marble_Circle;
            Marble_Circle.Previous.Next := Marble_Circle;
            Current_Marble              := Marble_Nr;

         end if;
         Current_Player := Current_Player + 1;
         if Current_Player > Players_In_Game then
            Current_Player := 1;
         end if;
      end;
   end loop;

   declare
      Max_Score : Long_Long_Integer := 0;
   begin
      for I in Player_Score_Table'Range loop
         if Player_Score_Table (I) > Max_Score then
            Max_Score := Player_Score_Table (I);
         end if;
      end loop;
      Put_Line ("Part 1:" & Max_Score'Img);
   end;

end Main;
