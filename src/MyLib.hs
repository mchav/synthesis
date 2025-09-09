{-# LANGUAGE GADTs #-}

module MyLib where

import qualified Data.List as L
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
gSize (Concat l r) = 1 + max (gSize l) (gSize r)
gSize (Substring start end v) = 1 + maximum [gSize start, gSize end, gSize v]
gSize (Find l r) = 1 + max (gSize l) (gSize r)
gSize _ = 1

generatePrograms :: String -> [Program String] -> [Program String]
generatePrograms expected xs = filter ((<= (length expected)) . length . interpret) (removeDupsLazy $ concatMap enumeratePrograms pairs)
    where
        pairs = [(p, q) | p <- xs, q <- xs]
        
        enumeratePrograms (p, q) = 
            [ Concat p q
            , Substring Start (Find p q) q
            , Substring Start (Find q p) q
            , Substring (Find p q) End q
            , Substring (Find q p) End q
            , Drop p
            , Drop q
            , p
            , q
            ]

removeDupsLazy :: Eq a => [a] -> [a]
removeDupsLazy = go []
    where
        go _ [] = []
        go seen (x:xs)
            | x `elem` seen = go seen xs
            | otherwise = x : go (x:seen) xs

interpretInt :: Program Int -> Maybe Int
interpretInt Start = Just 0
interpretInt End = Just 1024
interpretInt (Find needle haystack) = 
    case (interpret needle, interpret haystack) of
        ("", _)  -> Nothing
        (_, "")  -> Nothing
        (n', h') -> 
            findIndex n' h' 0
    where
        findIndex _ [] _ = Nothing
        findIndex n (h:hs) i
            | n `L.isPrefixOf` (h:hs) = Just i
            | otherwise = findIndex n hs (i + 1)

interpret :: Program String -> String
interpret (SValue v) = v
interpret (Drop v) = drop 1 (interpret v)
interpret (Concat l r) = interpret l ++ interpret r
interpret (Substring i j g) = 
    case (interpretInt i, interpretInt j) of
        (Nothing, _) -> ""
        (_, Nothing) -> ""
        (Just i', Just j') -> 
            let s = interpret g
            in take (j' - i') (drop i' s)

search :: String -> String -> Int -> Either String (Program String)
search i o d = 
    case searchStream [SValue i, SValue " ", SValue "."] o d of
        Nothing -> Left "No programs found"
        Just p -> Right p

searchStream :: [Program String] -> String -> Int -> Maybe (Program String)
searchStream ps expected d
    | d == 0 = Nothing
    | otherwise = 
        case findFirst ps of
            Just p -> Just p
            Nothing -> searchStream (generatePrograms expected ps) expected (d - 1)
    where
        findFirst [] = Nothing
        findFirst (p:ps')
            | interpret p == expected = 
                trace ("\nFound: " ++ show p) $ Just p
            | otherwise = findFirst ps'
