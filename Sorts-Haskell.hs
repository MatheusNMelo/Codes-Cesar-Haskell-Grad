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


programa:: String -> String -> IO()
programa x y | (x == "insertionsort") = ler_arq y
            | (x == "selectionsort") = ler_arq2 y
            | (x == "bubblesort") = ler_arq3 y
