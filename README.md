# Haskell_Test
import Data.List hiding (nub)
import Data.List (nub,sort)
import qualified Data.Map as M
import Data.Char
doubleMe x = x + x
doubleUs x y = x+x + y+y
doubleUS x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x<100 then x*2 else x
doubleSmallNuber' x = (if x<100 then x*2 else x)+1
boomRangs xl = [if x<10 then "BANG" else "BOOM"|x<-xl,x/=13, x/=15, x/=19,odd x]
removeNonUppercase st = [c|c<-st,c `elem` ['A'..'Z']]
sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = factorial(n-1) * n
charName :: Char ->String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Choolps"
charName x = "No Name"
addVectors :: (Num a) => (a,a) -> (a,a) ->(a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)
first :: (a,b,c) ->a
first (x1,_,_) = x1
second :: (a,b,c) ->b
second (_,y1,_) = y1
third :: (a,b,c) -> c
third (_,_,z1) = z1
head' :: [a] -> a
head' [] = error "Can't call head on an empty list,dummy!"
head' (x:_) = x
tell :: (Show a) => [a] ->String
tell [] = "Tell list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements :" ++ show x ++ "and" ++ show y
tell (_:x:y) = "The list is long, the last two elements are:" ++ show x ++ "  and" ++ show y
length' :: (Num b) =>[a] ->b
length' [] = 0
length' (_:x) = 1 + length' x
sum' :: (Num a) => [a] ->a
sum' [] = 0
sum' (x:xs) = x + sum' xs
capital :: String -> String
capital "" = "The String is empty"
capital all@(x:xs) = "the first capital of " ++ all ++" is "++[x]
bmiTell :: (RealFloat a) => a ->String
bmiTell bmi
  | bmi <= 18.5 ="You're underweight , aren't you?"
  | bmi <= 25.5 = "You're in weight , aren't you?"
  | bmi <=30.0 = "You're fat, lose some weight, fattly"
  | otherwise = "You're a whate, Congratuation"
bmiTell' :: (RealFloat a) => a ->a ->String
bmiTell' weight height
  | bmi <= skininy ="You're underweight , aren't you?"
  | bmi <= normal ="You're in weight , aren't you?"
  | bmi <= fat = "You're fat, lose some weight, fattly"
  | otherwise = "You're a whate, Congratuation"
  where bmi = weight / height^2
        skininy = 18.5
	normal = 25
	fat = 30

maxium' :: (Ord a) =>[a] ->a
maxium' [] = error "the list is empty"
maxium' [x] = x
maxium' (x:xs)
  | x > maxTail = x
  |otherwise = maxTail
  where maxTail = maxium' xs
maxium'' :: (Ord a) =>[a] ->a
maxium'' [] = error "the list is empty"
maxium'' [x] = x
maxium'' (x:xs) = max x (maxium'' xs)
replicate' :: (Num i,Ord i) => i -> a ->[a]
replicate' n x
  | n<=0 = []
  |otherwise = x:replicate' (n-1) x
take' :: (Num i,Ord i) => i ->[a] ->[a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
compareWithHundred :: (Num a, Ord a) => a ->Ordering
compareWithHundred = compare 100
divideByTen :: (Floating a) => a->a
divideByTen = (/10)
isUpperAlphanum :: Char ->Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
applyTwice :: (a->a) ->a ->a
applyTwice f x = f (f x)
zipWith' :: (a->b->c) ->[a] ->[b] ->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys
flip' :: (a->b->c) -> (b-> a ->c)
flip' f x y = f y x
fitter' :: (a ->Bool) -> [a] ->[a]
fitter' _[] = []
fitter' p (x:xs)
  | p x = x:fitter' p xs
  |otherwise = fitter' p xs
elem' :: (Eq a) =>a ->[a] ->Bool
elem' y ys = foldl (\add x -> if x==y then True else add) False ys
map' :: (a->b) ->[a] ->[b]
map' f xs = foldr (\x acc ->f x : acc) [] xs
map'' :: (a->b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
filt :: (a->Bool) -> [a] ->[a]
filt p = foldr(\x acc -> if p x then x :acc else acc) []
hea' :: [a] ->a
hea' = foldl1 (\x _->x)
last' :: [a] -> a
last' = foldr1 (\_ x->x)
search :: (Eq a) => [a] ->[a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x ->if take nlen x == needle then True else acc) False (tails haystack)
encode :: Int -> String -> String
encode shift msg = 
  let ords = map ord msg
      shifted = map (+shift) ords
    in map chr shifted
finderKey :: (Eq k) =>k -> [(k,v)] ->Maybe v
finderKey key [] = Nothing
finderKey key ((k,v):xs) =
  if key == k then Just v
  else finderKey key xs
finderKey' :: (Eq k) =>k ->[(k,v)] ->Maybe v
finderKey' key = foldl (\val (k,v) -> if key==k then Just v else val) Nothing
