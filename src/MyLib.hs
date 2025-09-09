{-# LANGUAGE GADTs #-}

module MyLib where

import qualified Data.List as L
-- import Data.Maybe
import Debug.Trace

data Program a where
    Concat :: Program String -> Program String -> Program String
    Substring :: Program Int -> Program Int -> Program String -> Program String
    Drop :: Program String -> Program String
    SValue :: String -> Program String
    Find :: Program String -> Program String -> Program Int
    Start :: Program Int
    End :: Program Int

instance (Eq a) => Eq (Program a) where
    (==) a@(Concat _ _) b@(Concat _ _)      = (interpret a) == (interpret b)
    (==) a@(Concat _ _) b@(Substring _ _ _) = (interpret a) == (interpret b)
    (==) a@(Concat _ _) b@(SValue _)      = (interpret a) == (interpret b)
    (==) a@(Concat _ _) b@(Drop _)      = (interpret a) == (interpret b)

    (==) a@(Substring _ _ _ ) b@(Concat _ _) = (interpret a) == (interpret b)
    (==) a@(Substring _ _ _ ) b@(Substring _ _ _) = (interpret a) == (interpret b)
    (==) a@(Substring _ _ _ ) b@(SValue _) = (interpret a) == (interpret b)
    (==) a@(Substring _ _ _ ) b@(Drop _) = (interpret a) == (interpret b)

    (==) a@(SValue _) b@(Concat _ _) = (interpret a) == (interpret b)
    (==) a@(SValue _) b@(Substring _ _ _) = (interpret a) == (interpret b)
    (==) a@(SValue _) b@(SValue _) = (interpret a) == (interpret b)
    (==) a@(SValue _) b@(Drop _) = (interpret a) == (interpret b)

    (==) a@(Drop _) b@(Concat _ _) = (interpret a) == (interpret b)
    (==) a@(Drop _) b@(Substring _ _ _) = (interpret a) == (interpret b)
    (==) a@(Drop _) b@(SValue _) = (interpret a) == (interpret b)
    (==) a@(Drop _) b@(Drop _) = (interpret a) == (interpret b)

    (==) a@(Find _ _) b@(Find _ _)      = (interpretInt a) == (interpretInt b)
    (==) a@(Find _ _) b@(Start) = (interpretInt a) == (interpretInt b)
    (==) a@(Find _ _) b@(End) = (interpretInt a) == (interpretInt b)

    (==) a@(Start) b@(Find _ _)      = (interpretInt a) == (interpretInt b)
    (==) a@(Start) b@(Start) = (interpretInt a) == (interpretInt b)
    (==) a@(Start) b@(End) = (interpretInt a) == (interpretInt b)

    (==) a@(End) b@(Find _ _)      = (interpretInt a) == (interpretInt b)
    (==) a@(End) b@(Start) = (interpretInt a) == (interpretInt b)
    (==) a@(End) b@(End) = (interpretInt a) == (interpretInt b)


instance (Show a) => Show (Program a) where
    show (SValue s) = show s
    show (Drop s) = "drop(" ++ show s ++ ")"
    show Start = "0"
    show End = "1024"
    show (Find n h) = "find(" ++ show n ++ ", " ++ show h ++ ")"
    show (Concat l r) = "(" ++ show l ++ " ++ " ++ show r ++ ")"
    show (Substring start end v) = "substring(" ++ show start ++ ", " ++ show end ++ ", " ++ show v ++ ")"

gSize :: Program a -> Int
gSize (Concat l r) = 1 + (max (gSize l) (gSize r))
gSize (Substring start end v) = 1 + max (max (gSize start) (gSize end)) (gSize v)
gSize (Find l r) = 1 + (max (gSize l) (gSize r))
gSize _ = 1

generatePrograms :: [Program String] -> [Program String]
generatePrograms xs = concatMap enumeratePrograms (allCombinations xs xs)
    where
        enumeratePrograms (p, q) = L.nub [ Concat p q
                                   , Substring Start (Find p q) q
                                   , Substring Start (Find q p) q
                                   , Substring (Find p q) End q
                                   , Substring (Find q p) End q
                                   , Drop p
                                   , Drop q
                                   , p
                                   , q
                                   ]

interpretInt :: Program Int -> Maybe Int
interpretInt Start    = Just 0
interpretInt End      = Just 1024
interpretInt (Find needle haystack) = let
        needle'    = interpret needle
        haystack'  = interpret haystack
        in case (needle', haystack') of
                    ("", "")        -> Nothing
                    ("", _)         -> Nothing
                    (_, "")         -> Nothing
                    (n'@(x:_), h')  -> case filter snd (map (\i -> (i, take (length n') (drop i h') == n')) (L.elemIndices x h')) of
                        []    -> Nothing
                        (i:_) -> Just (fst i)

interpret :: Program String -> String
interpret (SValue v) = v
interpret (Drop v) = drop 1 (interpret v)
interpret (Concat l r) = (interpret l) ++ (interpret r)
interpret (Substring i j g) = let
        s = interpret g
    in case (interpretInt i, interpretInt j) of
        (_, Nothing)       -> ""
        (Nothing, _)       -> ""
        (Just i', Just j') ->  drop i' (take j' s)

search :: String    -- | Input
       -> String    -- | Expected Output
       -> Int       -- | Max depth
       -> Either String (Program String)
search i o d = case search' [SValue i, SValue " ", SValue "."] o d of
    [] -> Left "No programs found"
    xs -> trace (show xs) $ case (L.sortBy (\a b -> compare (gSize a) (gSize b)) xs) of
        (p:_) -> Right p
        []    -> Left "Literally impossible"

search' :: [Program String] -> String -> Int -> [Program String]
search' ps expected d
    | d == 0             = []
    | not (null working) = working ++ (search' (generatePrograms ps) expected (d - 1))
    | otherwise          = search' (generatePrograms ps) expected (d - 1)
        where
            working = trace ("\n---------\n" ++ (L.intercalate "\n" (map show ps))) $ filter ((==expected) . interpret) ps 

allCombinations :: [a] -> [b] -> [(a, b)]
allCombinations xs ys = [(x, y) | x <- xs, y <- ys]
