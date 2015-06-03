add a b = a + b

myDrop n xs =   if n <= 0 || null xs
                then xs
                else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = if length xs == 2
                then head xs
                else lastButOne (tail xs)

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]