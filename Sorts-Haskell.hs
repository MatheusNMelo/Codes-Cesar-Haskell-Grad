import System.IO

--Isort

inserir :: (Ord a) => a -> [a] -> [a]
inserir x [] = [x]
inserir x (y:ys) | x <= y = x:y:ys 
                 | otherwise = y:(inserir x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = inserir x (insertionSort xs)

--Bsort

sort :: (Ord a) => [a] -> [a]
sort (x:y:xs)| x > y = y : sort (x:xs)
             | otherwise = x : sort (y:xs)
sort x = x

compareElem :: (Ord a) => [a] -> Int -> [a]
compareElem  x y | y == (length x) = x
                 | otherwise = compareElem (sort x) (y + 1) 
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort x = compareElem x 0

--Ssort

min1 :: (Ord a) => [a] -> a
min1 [x] = x
min1 (x:y:ys) | x <= y = min1 (x:ys)
              | otherwise = min1 (y:ys)

remover :: (Ord a) => a -> [a] -> [a]
remover x [] = []
remover x (y:ys) | x == y = ys
                 | otherwise = y:(remover x ys)


selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = m:(selectionSort ys)
    where 
        m = min1 xs 
        ys = remover m xs

--Msort

dividir :: (Ord a) => [a] -> ([a],[a])
dividir [] = ([],[])
dividir [x] = ([x],[])
dividir (x:y:xs) = ((x:ys), y:zs)
    where (ys, zs) = dividir xs

juntar :: (Ord a) => [a] -> [a] -> [a]
juntar xs [] = xs
juntar [] xs = xs
juntar (x:xs)(y:ys) | x < y = x:(juntar xs (y:ys))
                    | otherwise = y:(juntar (x:xs) ys)

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = juntar (msort ys) (msort zs)
    where (ys, zs) = dividir xs

--Qsort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

--Arquivos
    
inverte :: String -> String 
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

numero :: Char -> [Char] -> [Int] -> Int
numero x (y:ys) (z:zs) | (x == y) = z
                       | otherwise = numero x ys zs

toint :: [Char] -> Int -> Int
toint [] _ = 0
toint (x:xs) y | ((numero x ['0'..'9'] [0..9]) == 0) = (toint xs (y + 1))
               | otherwise = (10^y)*(numero x ['0'..'9'] [0..9]) + (toint xs (y + 1))

trans :: String -> Int
trans xs = toint (inverte xs) 0

ler_arq :: String -> IO() 
ler_arq y = do
    arq <- openFile y ReadMode
    conteudo <- hGetContents arq
    print (insertionSort(map (trans) (words conteudo)))
    hClose arq

ler_arq2 :: String -> IO() 
ler_arq2 y = do
    arq <- openFile y ReadMode
    conteudo <- hGetContents arq
    print (selectionSort(map (trans) (words conteudo)))
    hClose arq    

ler_arq3 :: String -> IO() 
ler_arq3 y = do
    arq <- openFile y ReadMode
    conteudo <- hGetContents arq
    print (bubblesort(map (trans) (words conteudo)))
    hClose arq    

ler_arq4 :: String -> IO() 
ler_arq4 y = do
    arq <- openFile y ReadMode
    conteudo <- hGetContents arq
    print (msort(map (trans) (words conteudo)))
    hClose arq       

ler_arq5 :: String -> IO() 
ler_arq5 y = do
    arq <- openFile y ReadMode
    conteudo <- hGetContents arq
    print (quicksort(map (trans) (words conteudo)))
    hClose arq      

programa:: String -> String -> IO()
programa x y | (x == "insertionsort") = ler_arq y
            | (x == "selectionsort") = ler_arq2 y
            | (x == "bubblesort") = ler_arq3 y
            | (x == "mergesort") = ler_arq4 y
            | (x == "quicksort") = ler_arq5 y

         
         
-- :set +s (Bota esse comando no GHCI que ele devolve o tempo de execução)

--programa "quciksort""1000.txt"
