module Main where

import Data.List

data NumberPart =
    Hundred Char
    | Somety Char Char
    | Someteen Char Char
    | None
    deriving (Show, Eq)


data Section = 
    S NumberPart NumberPart
    deriving Show



tosentance::[String] -> String
tosentance = foldr (++) ""

sectionwords:: String -> [String]
sectionwords = addSpace . addSuffixes . modifyLast . (map towords) . to3digits . padToMultipleof3

modifyLast:: [String] -> [String]
modifyLast [] = []
modifyLast xs = let 
                    l = length xs
                    lastSection = last xs 
                    shouldAppendAnd = (l /= 1) &&  not ("and" `isInfixOf` lastSection)
                in
                    if shouldAppendAnd then
                        (init xs) ++  ["and " ++ lastSection]
                    else
                        xs

addSpace::[String] -> [String]
addSpace [] = []
addSpace (x:y:xs) = x : map (" " ++) (y:xs)
addSpace (x:xs) = (x:xs)


addSpaceToWord::Int -> String -> String
addSpaceToWord n s 
    | n == 0 = s
    | otherwise = " " ++ s

addSuffixes :: [String] -> [String]
addSuffixes s = reverse [x ++ (if i /= "" then " " else "") ++ i 
                    | (x, i) <- zip (reverse s) 
                        ["", "thousand", "million", "billion", "trillion"]
                    , x /= ""]

padToMultipleof3 :: String -> String
padToMultipleof3 s = (replicate n '0') ++ s 
            where
                d = ((length s) `mod` 3)
                n = if (d == 0) then 0 else (3 - d)

to3digits :: String -> [Section]
to3digits []  = []
to3digits (h:t:u:xs) = (tosection h t u)  : to3digits xs

tosection :: Char -> Char -> Char -> Section
tosection h t u = S (tohundreds h) (totensunits t u)


tohundreds :: Char -> NumberPart 
tohundreds a = if a == '0' then None else Hundred a

totensunits :: Char -> Char -> NumberPart
totensunits b c  
    | b == '0' && c == '0' = None
    | b == '0' && c /= '0' = Someteen b c
    | b == '1'             = Someteen b c
    | otherwise            = Somety b c


towords :: Section -> String
towords (S a b) =
        if (a == None) then
            if (b == None) then
                ""
            else
                parttowords b
        else
            if (b == None) then
                parttowords a
            else
                (parttowords a) ++ " and " ++ (parttowords b)

parttowords :: NumberPart -> String
parttowords s =
    case s of
        Hundred a   -> (translateunits a) ++ " hundred"
        Somety a b  -> (translatetens a) ++ if (b /= '0') then 
                                                " " ++ (translateunits b)
                                            else
                                                ""
        Someteen a b -> if (a=='0') then 
                            translateunits b
                        else
                            translateteens [a,b]
        None -> ""
          
translateunits :: Char -> String
translateunits s =
    case s of
        '1' -> "One"
        '2' -> "Two"
        '3' -> "Three"
        '4' -> "Four"
        '5' -> "Five"
        '6' -> "Six"
        '7' -> "Seven"
        '8' -> "Eight"
        '9' -> "Nine"
        otherwise -> "oops"

translateteens :: String -> String
translateteens s =   
    case s of     
        "10" -> "Ten"
        "11" -> "Eleven"
        "12" -> "Twelve"
        "13" -> "Thirteen"
        "14" -> "Fourteen"
        "15" -> "Fifteen"
        "16" -> "Sixteen"
        "17" -> "Seventeen"
        "18" -> "Eighteen"
        "19" -> "Nineteen"
        otherwise -> "oops"
          
translatetens :: Char -> String
translatetens s =
    case s of
        '2' -> "Twenty"
        '3' -> "Thirty"
        '4' -> "Fourty"
        '5' -> "Fifty"
        '6' -> "Sixty"
        '7' -> "Seventy"
        '8' -> "Eighty"
        '9' -> "Ninety"
        otherwise -> "oops"
        

main :: IO()
main = do
    putStrLn "type in a number"
    inp <- getLine
    putStrLn $ (show . sectionwords) inp