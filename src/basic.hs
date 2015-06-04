add a b = a + b

myDrop n xs =   if n <= 0 || null xs
                then xs
                else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs = if length xs == 2
                then head xs
                else lastButOne (tail xs)

data BookInfo = Book Int String [String]
              | ABook Int String String
                deriving (Show)

data MagazineInfo = Magazine Int String
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)

myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList [] = 0

author (Book id name authors) = name

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
} deriving (Show)

data List a = Cons a (List a)
            | Nil
            deriving (Show)

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Show)

shouldLend balance amount = let reserve = 100
                                newBalance = balance - amount
                            in if newBalance < reserve
                            then Nothing
                            else Just amount

fromList (Cons a xs) = a : fromList(xs)
fromList Nil = []

data Tree2 a = Tree2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
            deriving (Show)

fromMaybe defval c =
    case c of
        Just value -> value
        _ -> defval

data Fruit = Apple | Orange
            deriving (Show)

apple = "apple"
orange = "orange"

whichFruit :: String -> Fruit
whichFruit f = case f of
                    "apple" -> Apple
                    "orange" -> Orange

lend amount balance
    | amount <= 0                   = Nothing
    | newBalance <= 0               = Nothing
    | otherwise                     = Just amount
    where reserve = 100
          newBalance = balance - reserve - amount