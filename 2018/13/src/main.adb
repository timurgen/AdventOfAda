with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;

procedure Main is

   type Direction is (UP, DOWN, LEFT, RIGHT);
   type Turns is (LEFT_TURN, STRAIGHT, RIGHT_TURN);
   for Direction'Size use 2;
   for Turns'Size use 2;

   function Get_Next_Turn
     (T : Turns) return Turns is
     (if T = LEFT_TURN then STRAIGHT
      elsif T = STRAIGHT then RIGHT_TURN
      else LEFT_TURN) with Inline;

   type Cart is record
      X         : Natural;
      Y         : Natural;
      D         : Direction;
      Next_Turn : Turns;
      Deleted: Boolean;
   end record;

   package Cart_V_P is new Indefinite_Vectors (Positive, Cart);
   Carts : Cart_V_P.Vector;

   function Cart_That_Should_Stay_First
     (Left, Right : Cart) return Boolean is
     (if Left.Y = Right.Y then Left.X < Right.X else Left.Y < Right.Y) with
     Inline;

   package Cart_Sorter is new Cart_V_P.Generic_Sorting
     ("<" => Cart_That_Should_Stay_First);

   type Grid is array (0 .. 150, 0 .. 150) of Character;
   Game_Grid : Grid := (others => (others => ' '));

   Input_File              : File_Type;
   Input_File_Line_Counter : Natural := 0;

   Bad_Direction : exception;
   Collision: exception;

   function Char_To_Direction (Ch : Character) return Direction is
   begin
      case Ch is
         when '^' =>
            return UP;
         when 'v' =>
            return DOWN;
         when '<' =>
            return LEFT;
         when '>' =>
            return RIGHT;
         when others =>
            raise Bad_Direction;

      end case;
   end Char_To_Direction;

   procedure Print_Cart_Data (Carts : Cart_V_P.Vector) is
      package IO is new Integer_IO (Positive);
   begin
      for I in Carts.First_Index .. Carts.Last_Index loop
         IO.Put (I, 2);
         Put (": ");
         IO.Put (Carts (I).X, 3);
         Put ("-");
         IO.Put (Carts (I).Y, 3);
         Put (" => ");
         Put_Line (Carts (I).D'Img);
      end loop;
   end Print_Cart_Data;

   procedure Turn_Left (C: in out Cart) is
   begin
      if C.D = UP then C.D := LEFT;
      elsif C.D = DOWN then C.D := RIGHT;
      elsif C.D = LEFT then C.D := DOWN;
      elsif C.D = RIGHT then C.D := UP;
      else raise Bad_Direction;
      end if;
   end Turn_Left;

   procedure Turn_Right (C: in out Cart) is
   begin
      if C.D = UP then C.D := RIGHT;
      elsif C.D = DOWN then C.D := LEFT;
      elsif C.D = LEFT then C.D := UP;
      elsif C.D = RIGHT then C.D := DOWN;
      else raise Bad_Direction;
      end if;
   end Turn_Right;

   function Is_Collision_Detected (C: in out Cart_V_P.Vector) return Boolean is
   begin
      for I in C.First_Index..C.Last_Index -1 loop
         for J in I+1..C.Last_Index loop
            if C(I).X = C(J).X and then C(I).Y = C(J).Y then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Is_Collision_Detected;

   procedure Remove_Colliding_Carts (C: in out Cart_V_P.Vector) is
   begin
      for I in C.First_Index..C.Last_Index -1 loop
         for J in I+1..C.Last_Index loop
            if C(I).X = C(J).X and then C(I).Y = C(J).Y and then not C(J).Deleted then
               C(I).Deleted := True;
               C(J).Deleted := True;
               return;
            end if;
         end loop;
      end loop;
   end Remove_Colliding_Carts;

   function Run_Carts (Carts : in out Cart_V_P.Vector; Part: Integer) return Boolean is
      use Cart_Sorter;
   begin
      Sort (Container => Carts);
      for I in Carts.First_Index .. Carts.Last_Index loop
         if not Carts(I).Deleted then
            declare
               Curr_Cart      : Cart := Carts.Element (I);
               Next_Step_Char : Character;
            begin
               if Curr_Cart.D = UP then
                  Curr_Cart.Y := Curr_Cart.Y - 1;
               elsif Curr_Cart.D = DOWN then
                  Curr_Cart.Y := Curr_Cart.Y + 1;
               elsif Curr_Cart.D = LEFT then
                  Curr_Cart.X := Curr_Cart.X - 1;
               elsif Curr_Cart.D = RIGHT then
                  Curr_Cart.X := Curr_Cart.X + 1;
               else
                  raise Bad_Direction;
               end if;

               Next_Step_Char := Game_Grid (Curr_Cart.X, Curr_Cart.Y);
               case Next_Step_Char is
               when '/' =>
                  if Curr_Cart.D = UP then
                     Curr_Cart.D := RIGHT;
                  elsif Curr_Cart.D = DOWN then
                     Curr_Cart.D := LEFT;
                  elsif Curr_Cart.D = LEFT then
                     Curr_Cart.D := DOWN;
                  elsif Curr_Cart.D = RIGHT then
                     Curr_Cart.D := UP;
                  end if;
               when '\' =>
                  if Curr_Cart.D = UP then
                     Curr_Cart.D := LEFT;
                  elsif Curr_Cart.D = DOWN then
                     Curr_Cart.D := RIGHT;
                  elsif Curr_Cart.D = LEFT then
                     Curr_Cart.D := UP;
                  elsif Curr_Cart.D = RIGHT then
                     Curr_Cart.D := DOWN;
                  end if;
               when '+' =>
                  if Curr_Cart.Next_Turn = LEFT_TURN then
                     Turn_Left(Curr_Cart);
                     Curr_Cart.Next_Turn := STRAIGHT;
                  elsif Curr_Cart.Next_Turn = STRAIGHT then
                     Curr_Cart.Next_Turn := RIGHT_TURN;
                  elsif Curr_Cart.Next_Turn = RIGHT_TURN then
                     Turn_Right(Curr_Cart);
                     Curr_Cart.Next_Turn := LEFT_TURN;
                  else
                     raise Bad_Direction;
                  end if;
               when others =>
                  null;
               end case;
               Carts(I) := Curr_Cart;
               if Is_Collision_Detected(C => Carts) then
                  if Part = 1 then
                     return True;
                  else -- part 2
                     Remove_Colliding_Carts(Carts);
                  end if;
               end if;
               -- part 2
               if Carts.Length = 1 then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Run_Carts;

   procedure Eliminate_Deleted_Carts (C: in out Cart_V_P.Vector) is
      use Cart_V_P;
      Curr: Cart_V_P.Cursor := C.First;
      Next: Cart_V_P.Cursor;
   begin
      loop
         exit when Curr = Cart_V_P.No_Element;
         declare
            Curr_Cart : constant Cart := Cart_V_P.Element (Curr);
         begin
            if not Curr_Cart.Deleted then
               Cart_V_P.Next (Position => Curr);
            else
               Next := Cart_V_P.Next (Curr);
               C.Delete (Position => Curr);
               Curr := Next;
            end if;
         end ;
      end loop;
   end Eliminate_Deleted_Carts;

   Part: Integer := 1;
begin
   Open (Input_File, In_File, "input.txt");

   loop
      declare
         Line : String := Get_Line (Input_File);
      begin
         for Char in Line'Range loop
            if Line (Char) in '/' | '\' | '+' then
               Game_Grid (Char - 1, Input_File_Line_Counter) := Line (Char);
            elsif Line (Char) in '^' | 'v' | '<' | '>' then
               Carts.Append
                 ((Char - 1,
                  Input_File_Line_Counter,
                  Char_To_Direction (Line (Char)),
                  LEFT_TURN, False));
            end if;
         end loop;
         Input_File_Line_Counter := Input_File_Line_Counter + 1;
      end;
      exit when End_Of_File (Input_File);
   end loop;
   Close (Input_File);

   loop
      -- Print_Cart_Data(Carts);
      exit when  Run_Carts(Carts, Part);
   end loop;
   Print_Cart_Data(Carts);
   Part := 2;
   loop
      -- Print_Cart_Data(Carts);
      Eliminate_Deleted_Carts(Carts);
      exit when  Run_Carts(Carts, Part);
   end loop;
   Print_Cart_Data(Carts);
end Main;
