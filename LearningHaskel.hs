{-
	Funciones generadas a partir del sitio web
	http://learnyouahaskell.com/introduction
-}

doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100 then x else x*2
conanO'Brien = "It's a.me, Conan O'Brien!"
-- length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- Factorial de un numero de manera recursiva
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- Funcion length' implementada con declaración explicita y usando recursividad
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guardias

bmiTell :: (RealFloat a) => a -> a ->String
bmiTell weight height
 | bmi <= skinny = "You're underweight, you emo, you!"
 | bmi <= normal = "You're supposedly normal, Pffft, I bet you're ugly"
 | bmi <= fat = "You're fat! Lose some weight, fatty!"
 | otherwise = "You're a whale, congrats!"
 where bmi = weight / height ^ 2
       skinny = 18.5
       normal = 25.0
       fat = 30.0
 
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 | a > b = GT
 | a == b = EQ
 | otherwise = LT
 
cylinder :: (RealFloat a) => a -> a-> a
cylinder r h = 
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^2
 in  sideArea + 2 * topArea


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
 let smallerSorted = quicksort [a | a <- xs, a <= x]
     biggerSorted = quicksort [a | a <- xs, a > x]
 in smallerSorted ++ [x] ++ biggerSorted