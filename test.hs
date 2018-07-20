ggT :: Int -> Int -> Int
ggT a 0 = a
ggT a b = ggT b (mod a b)


m :: Int -> Int
m n | n > 100 = n - 10
    | n <= 100 = m ( m (n + 11))

prim :: Int -> Bool
prim n | n < 2  = False
       | n == 2 = True
       | otherwise = prim' n (n-1)

prim' :: Int -> Int -> Bool
prim' n 1 = True   
prim' n i | mod n i == 0 = False
          | otherwise = prim' n (i-1)


f1 :: Int -> Int
f1 1 =          2
f1 2 = (f1 1) + 4
f1 3 = (f1 2) + 6
f1 4 = (f1 3) + 8 
f1 n = (f1 (n-1)) + (2*n)

f2 :: Int -> Int
f2 1 = 1
f2 n = f2 (n-1) + (2*n-1)

f3 :: Int -> Double -> Double
f3 1 q = 1
f3 n q = f3 (n-1) q + q^n

lastInt :: [Int] -> Int
lastInt [] = 0
lastInt (x:[]) = x
lastInt (x:xs) = lastInt xs

maxInt :: [Int] -> Int
maxInt [] = 0
maxInt (x:[]) = x
maxInt (x:y:[]) | x > y = x
                | otherwise = y
maxInt (x:y:xs) | x > y = maxInt (x:xs)
                | otherwise = maxInt (y:xs)

repl :: Int -> Int -> [Int]
repl 0 n = []
repl 1 n = [n]
repl i n = n:(repl (i-1) n) 


replace :: Int -> Int -> [Int] -> [Int]
replace r n [] = []
replace r n (x:[]) | r == x    = (n:[])
                   | otherwise = (x:[])
replace r n (x:xs) | r == x    = n:(replace r n xs)
                   | otherwise = x:(replace r n xs)
                

prod :: [Int] -> Int
prod [] = 1
prod (x:[]) = x
prod (x:xs) = x * (prod xs)


primes :: Int -> [Int]
primes n | n < 2     = []
         | prim n    = n:(primes (n-1))
         | otherwise = primes (n-1)


fromTo :: Int -> Int -> [Int]
fromTo n m | n < m  = n:(fromTo (n+1) m)
           | n > m  = n:(fromTo (n-1) m)
           | n == m = [m]