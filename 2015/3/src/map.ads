with Ada.Containers.Hashed_Maps; use Ada.Containers;
package Map is

      type Coordinates is record
      X : Integer;
      Y : Integer;
   end record;

   type Integer_Acc is access Integer;

   function Get_Hash (Id : Coordinates) return Hash_Type;
   
   package Visited_Houses is new Ada.Containers.Hashed_Maps
     (Key_Type => Coordinates, Element_Type => Integer_Acc, Hash => Get_Hash,
      Equivalent_Keys => "=");

end Map;
