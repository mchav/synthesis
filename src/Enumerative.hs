{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Enumerative where

import Data.Char
import qualified Data.List as L
import Debug.Trace (trace)

data Program a where
    Concat :: Program String -> Program String -> Program String
    Substring :: Program Int -> Program Int -> Program String -> Program String
    Tail :: Program String -> Program String
    Head :: Program String -> Program String
    Lower :: Program String -> Program String
    Upper :: Program String -> Program String
    SValue :: String -> Program String
    Variable :: Program String
    Find :: Program String -> Program String -> Program Int
    Start :: Program Int
    End :: Program Int

instance Show (Program String -> Program String) where
    show f = "(\\Variable -> " ++ show (f Variable) ++ ")"

instance (Eq a) => Eq (Program a) where
    -- \| String cases
    (==) a@(Concat _ _) b = interpret a == interpret b
    (==) a@(Substring{}) b = interpret a == interpret b
    (==) a@(SValue _) b = interpret a == interpret b
    (==) a@(Tail _) b = interpret a == interpret b
    (==) a@(Lower _) b = interpret a == interpret b
    (==) a@(Upper _) b = interpret a == interpret b
    (==) a@(Head _) b = interpret a == interpret b
    -- \| Int cases
    (==) a@(Find _ _) b = interpretInt a == interpretInt b
    (==) a@Start b = interpretInt a == interpretInt b
    (==) a@End b = interpretInt a == interpretInt b
    (==) _ _ = False

deriving instance Show (Program String)
deriving instance Show (Program Int)

gSize :: Program a -> Int
gSize (Concat l r) = 1 + max (gSize l) (gSize r)
gSize (Substring start end v) = 1 + maximum [gSize start, gSize end, gSize v]
gSize (Find l r) = 1 + max (gSize l) (gSize r)
gSize (Tail v) = 1 + gSize v
gSize (Head v) = 1 + gSize v
gSize (Lower v) = 1 + gSize v
gSize (Upper v) = 1 + gSize v
gSize _ = 1

comp :: (b -> c) -> (a -> b) -> a -> c
comp = (.)

generatePrograms :: [Program String] -> [Program String -> Program String] -> [Program String -> Program String]
generatePrograms vars existingPrograms = L.sortBy (\p q -> gSize (p (SValue "")) `compare` gSize (q (SValue "")))
    [ comp transform p 
    | p <- existingPrograms
    , transform <- [Tail, Head, Lower, Upper]
    ] ++
    [ comp p q
    | p <- existingPrograms
    , q <- existingPrograms
    ] ++
    [ \v -> Concat (p v) (q v)
    | p <- existingPrograms
    , q <- existingPrograms
    ] ++
    [ comp t p
    | v <- vars
    , p <- existingPrograms
    , t <- transformsWithVar v
    ] ++
    [ comp p t
    | v <- vars
    , p <- existingPrograms
    , t <- transformsWithVar v
    ]
  where
    transformsWithVar v =
        [ \v' -> Concat v v'
        , \v' -> Concat v' v
        , \v' -> Substring Start (Find v v') v'
        , \v' -> Substring Start (Find v' v) v
        , \v' -> Substring (Find v v') End v'
        , \v' -> Substring (Find v' v) End v
        ]

equivalent :: [String] -> (Program String -> Program String) -> (Program String -> Program String) -> Bool
equivalent inputs p1 p2 = all ((\i -> interpret (p1 i) == interpret (p2 i)) . SValue) inputs

deduplicate :: [String] -> [Program String -> Program String] -> [Program String -> Program String]
deduplicate inputs = go []
  where
    go _ [] = []
    go seen (x : xs)
        | any (equivalent inputs x) seen = go seen xs
        | otherwise = x : go (x : seen) xs

interpretInt :: Program Int -> Maybe Int
interpretInt Start = Just 0
interpretInt End = Just (-1)
interpretInt (Find needle haystack) =
    case (interpret needle, interpret haystack) of
        ("", _) -> Nothing
        (_, "") -> Nothing
        (n', h') ->
            findIndex n' h' 0
  where
    findIndex _ [] _ = Nothing
    findIndex n (h : hs) i
        | n `L.isPrefixOf` (h : hs) = Just i
        | otherwise = findIndex n hs (i + 1)

interpret :: Program String -> String
interpret (SValue v) = v
interpret Variable = error "Cannot evaluate expression with free variable" 
interpret (Tail v) = drop 1 (interpret v)
interpret (Head v) = take 1 (interpret v)
interpret (Lower v) = map toLower (interpret v)
interpret (Upper v) = map toUpper (interpret v)
interpret (Concat l r) = interpret l ++ interpret r
interpret (Substring start end g) =
    case (interpretInt start, interpretInt end) of
        (Nothing, _) -> ""
        (_, Nothing) -> ""
        (Just i, Just j) ->
            let
                s = interpret g
                i' = if i == (-1) then length s else i
                j' = if j == (-1) then length s else j
             in
                take (j' - i') (drop i' s)

search :: [(String, String)] -> Int -> Either String (Program String -> Program String)
search examples d =
    case searchStream examples (generateVariables examples) [] d of
        Nothing -> Left "No programs found"
        Just p -> Right p

{- | This should create variables out of the inputs and strings in the output that aren't
| in the input.
-}
generateVariables :: [(String, String)] -> [Program String]
generateVariables examples = map (SValue . fst) examples ++ L.nub (commonInput ++ commonOutput)
  where
    inputs = map (map toLower . fst) examples
    outputs = map (map toLower . snd) examples
    inputChars = concat inputs
    outputChars = concat outputs
    commonInput = map (SValue . L.singleton) (filter (\c -> all (L.isInfixOf [c]) inputs) inputChars)
    commonOutput = map (SValue . L.singleton) (filter (\c -> all (L.isInfixOf [c]) outputs) outputChars)

searchStream ::
    -- | Examples
    [(String, String)] ->
    -- | Variables
    [Program String] ->
    -- | Programs
    [Program String -> Program String] ->
    -- | Search depth
    Int ->
    Maybe (Program String -> Program String)
searchStream examples variables programs d
    | d == 0 = Nothing
    | otherwise = trace (L.intercalate "\n" $ map show ps) $
        case findFirst ps of
            Just p -> Just p
            Nothing -> trace (L.intercalate "\n" $ map show ps) $ searchStream examples variables (generatePrograms variables ps) (d - 1)
  where
    ps = trace (L.intercalate "\n" $ map show programs) $ deduplicate (map fst examples) (if null programs then [id] else programs)
    findFirst [] = Nothing
    findFirst (p : ps')
        | all (\(i, o) -> interpret (p (SValue i)) == o) examples = trace ("\nFound: " ++ show p) $ Just p
        | otherwise = findFirst ps'
