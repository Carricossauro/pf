import System.Random
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x l = aux 0 x l
				where
					aux :: Eq a => Int -> a -> [a] -> [Int]
					aux n x (h:t) | h == x = n:aux (n+1) x t
								  | otherwise = aux (n+1) x t
					aux n x [] = []

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) l | elem h l = isSubsequenceOf' t (drop (head (elemIndices h l)) l)
						 | otherwise = False

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
x = Node (10,5) (Node (7,4) (Node (2,3) Empty Empty) (Node (8,2) Empty Empty)) (Node (42,1) Empty Empty) --Lista de procura

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP a (Node (x,y) e d) | a == x = Just y
							| a < x = lookupAP a e
							| otherwise = lookupAP a d


zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node r e d) (Node a x y) = Node (f r a) b c
									where
										b = zipWithBT f e x
										c = zipWithBT f d y

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (h:a,b)
				 | isAlpha h = (a,h:b)
			where
				(a,b) = digitAlpha t

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a) deriving Eq
a = App (App (Cons 7 (Cons 5 Nil)) (Cons 3  Nil)) (Cons 1 Nil)

firstSeq :: Seq a -> a
firstSeq Nil = error "Nope"
firstSeq (Cons a Nil) = a
firstSeq (Cons a b) = firstSeq b
firstSeq (App Nil b) = firstSeq b
firstSeq (App a Nil) = firstSeq a

dropSeq :: Eq a => Int -> Seq a -> Seq a
dropSeq x Nil = Nil
dropSeq x (Cons a b) | altura b == x = normal (Cons a Nil)
					 | otherwise = normal (Cons a (dropSeq x b))
dropSeq x (App a b) | a == Nil = dropSeq x b
					| altura a == x = normal (App Nil b)
					| otherwise = normal (App (dropSeq x a) b)

altura :: Seq a -> Int
altura Nil = 0
altura (Cons a b) = 1 + altura b
altura (App a b) = 1 + altura a

normal :: Seq a -> Seq a
normal Nil = Nil
normal (Cons a b) = Cons a (normal b)
normal (App Nil b) = normal b
normal (App a Nil) = normal a
normal (App a b) = App (normal a) (normal b)

instance Show a => Show (Seq a) where
	show Nil = "<<>>"
	show (Cons a b) = "<<" ++ show a ++ show2 b ++ ">>"
					where
						show2 :: Show a => Seq a -> String
						show2 Nil = ""
						show2 (Cons a b) = "," ++ show a ++ show2 b
						show2 (App a b) = show2 a ++ show2 b
	show (App a b) = "<<" ++ show2 a ++ show2 b ++ ">>"
				where
					show2 :: Show a => Seq a -> String
					show2 Nil = ""
					show2 (Cons a Nil) = show a ++ ","
					show2 (Cons a b) = show a ++ "," ++ show2 b
					show2 (App a b) = show2 a ++ show2 b

type Mat a = [[a]]
{-
magic :: Mat Int -> Bool
magic [] = True
magic p = linhas p x && colunas l x && diagonais p x
	where
		l = map head p
		x = sum l
		colunas t n = all (==n) (map sum (map head t)) && colunas (map init t) n
		linhas a b = all (==b) (map sum a)
		diagonais c d = let (e,f) = ldia c 0
						 in sum e == d && sum f == d
		ldia (h:t) x | length (h:t) == x = [(h!!x)]
				     | otherwise = (h!!x):ldia t (x+1)
-}