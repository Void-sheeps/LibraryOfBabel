type H_Array is array (0 .. 7) of Word32;

H : constant H_Array := (
   16#6a09e667#,  -- sqrt(2)
   16#bb67ae85#,  -- sqrt(3)
   16#3c6ef372#,  -- sqrt(5)
   16#a54ff53a#,  -- sqrt(7)
   16#510e527f#,  -- sqrt(11)
   16#9b05688c#,  -- sqrt(13)
   16#1f83d9ab#,  -- sqrt(17)
   16#5be0cd19#   -- sqrt(19)
);
