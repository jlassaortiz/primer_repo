mcd:: Integer -> Integer -> Integer
mcd a b | a < b = mcd b a
mcd a 0 = a
mcd a b = mcd b (mod a b)
