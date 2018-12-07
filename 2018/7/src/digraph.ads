with Ada.Containers.Indefinite_Ordered_Sets; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
package Digraph is

   function Char_Compare(Left,Right: Character) return Boolean is (Left < Right);
   function Char_Equal(Left,Right: Character) return Boolean is (Left = Right);
     
   
   package Char_Set_P is new Indefinite_Ordered_Sets(Element_Type => Character,
                                                     "<"          => Char_Compare,
                                                     "="          => Char_Equal);
   
   function Set_Equal(Left, Right: Char_Set_P.Set) return Boolean is (Left = Right);
   
   package Symbol_Table_P is new Indefinite_Ordered_Maps(Key_Type     => Character,
                                                         Element_Type => Char_Set_P.Set,
                                                         "<"          => Char_Compare,"=" => Set_Equal);
   

   
end Digraph;
