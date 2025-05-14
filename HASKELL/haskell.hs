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

größer :: Int -> Int -> Bool
größer x y
| x > y = True
| otherwise = False

entfernen :: [Int] -> Int -> [Int]
entfernen [] _ = error "Indexfehler!"
entfernen (x:xs) 0 = xs
entfernen (x:xs) i = x:entfernen xs (i-1)

einfügen :: [Int] -> Int -> Int -> [Int]
einfügen [] 0 y = [y]
einfügen [] _ _ = error "Indexfehler!"

anzahl :: [Int] -> Int -> Int
anzahl [] = 0
anzahl (x:xs) ys
    |x == y = 1 + anzahl xs + y
    | otherwise = anzahl xs y

größtes :: [Int] -> Int
größtes [] = error "Liste leer!"
größtes [x] = x
größtes (x:xs) 
    | x > größtes_xs = x
    | otherwise = größtes_xs
    where größtes_xs = größtes xs

kleinstes :: [Int] -> Int
kleinstes [] = error "Liste leer"
kleinstes [x] = xs
kleinstes (x:xs)
    | x < kleintes_xs = x
    | otherwise = kleinstes_xs
    where kleinstes_xs = kleinstes xs

enthält :: [Int] -> Int -> [Int]
enthält x y = anzahl x y > 0

löschen :: [Int] -> Int -> [Int]
löschen [] _ = error "Liste leer!"
löschen (x:xs) y
    | x == y = xs
    | otherwise = x:löschen xs y

löschenAlle :: [Int] -> Int -> [Int]
löschenAlle [] = error "Liste leer!"
löschenAlle (x:xs) y
    | x == y löschenAlle xs y 
    | otherwise = x:löschenAlle xs y

index

umdrehen

sortiert

einsBisN

sortieren