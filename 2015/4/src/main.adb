with Ada.Text_IO; use Ada.Text_IO;
with GNAT.MD5;    use GNAT.MD5;
with Ada.Strings.Fixed;
procedure Main is
   Input : String := "bgvyzdsv";
begin
   for I in 0 .. Integer'Last loop
      if Digest (Input & Ada.Strings.Fixed.Trim (Integer'Image (I), Ada.Strings.Left)) (1 .. 6) = "000000" then
         Put_Line (I'Img);
         exit;
      end if;

   end loop;
end Main;
