package body Map is

   function Get_Hash (Id : Coordinates) return Hash_Type is
      Hash : Integer := 0;
   begin
      Hash := ((Id.X + Id.Y) * (Id.X + Id.Y + 1)) / 2 + Id.Y;
      return Hash_Type'Val (Hash);
   end Get_Hash;

end Map;
