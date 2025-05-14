mal2 :: Int -> Int
mal2 n = n * 2

quadrat :: Int -> Int
quadrat n = n * n

fakultät :: Int -> Int
fakultät 0 = 1
fakultät n = fakultät (n-1) * n

zweiHoch :: Int -> Int
zweiHoch 0 = 1
zweiHoch n = 2 * zweiHoch (n-1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

summe_0_bis :: Int -> Int
summe_0_bis 0 = 0
summe_0_bis n = n + summe_0_bis (n-1)


hoch :: Int -> Int -> Int
hoch n m = n ^ m

binomialkoeffizient :: Int -> Int -> Int
binomialkoeffizient n k = div (fakultät n) (fakultät k * fakultät (n-k))

ackermann :: Int -> Int -> Int
ackermann 0 m = m + 1
ackermann n 0 = ackermann (n-1) * 1
ackermann n m = ackermann (n - 1) * ackermann( n (m - 1))

erstes :: [Int] -> Int 
erstes [] = error "leere Liste"
erstes (x:xs) = xs

letztes :: [Int] -> Int
letztes [] = error "leere Liste"
letztes [x] = x
letztes (x:xs) = letztes xs

-- zweites :: [Int] -> Int
-- zweites [] =  error "Liste zu kurz!"
-- zweites [x] = error "Liste zu kurz!"
-- zweites (x:y:ys) = y

zweites :: [Int] -> Int
zweites (x:y:ys) = y
zweites _ = error "Liste zu kurz!"

länge :: [Int] -> Int
länge [] = 0
länge (x:xs) = länge xs + 1


summe :: [Int] -> Int 
summe [] = 0
summe (x:xs) = x + summe xs

gib :: [Int] -> Int -> Int
gib []= error "Liste leer"
gib (x:xs) 0 = 1  
gib (x:xs) i = gib xs (i-1)

--entfernen :: [Int] -> Int -> Int

--einfügen :: [Int] -> Int -> Int -> [Int]