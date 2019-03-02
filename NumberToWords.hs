module NumberToWords 
(
    toSentance
)
where

import Data.List

data TensUnitsPart =
    Somety Char Char
    | Someteen Char Char
    | NoTensUnits
    deriving (Show, Eq)

data HundredsPart =
    Hundred Char
    | NoHundreds
    deriving (Show, Eq)

data Section = 
    S HundredsPart TensUnitsPart
    deriving Show


suffixes = ["", "thousand", "million", "billion", "trillion"]

toSentance:: String -> String
toSentance = appendWords . toWordsList

appendWords::[String] -> String
appendWords = foldr (++) ""

toWordsList::String -> [String]
toWordsList = (map tupleToWords) . sectionTuples

tupleToWords::(String, String, Section) -> String
tupleToWords (suffix, sectionSep, section) = (towords section) ++ suffix ++ sectionSep

sectionTuples:: String -> [(String, String, Section)]
sectionTuples = addSuffixes . buildSep . tokenize



buildSep::[Section] -> [(String, Section)]
buildSep [] = []
buildSep (x:[]) = [("", x)]
buildSep (x:y:[]) = [(finalSep y, x), ("", y)]
buildSep (x:y:xs) = (" ", x) : (buildSep (y:xs))

finalSep::Section -> String
finalSep (S NoHundreds NoTensUnits) = ""
finalSep (S (Hundred _) _ ) = " "
finalSep (S NoHundreds _) = " and "


tokenize :: String -> [Section]
tokenize = tosectionlist . padToMultipleof3


addSuffixes :: [(String, Section)] -> [(String, String, Section)]
addSuffixes s = let
                    l = length s
                in
                    [ build x (l - i) | (x, i) <- zip s [1..], 
                        let (sectionSep, S a b) = x
                        in (a /= NoHundreds || b /= NoTensUnits)]

build::(String, Section)->Int->(String, String, Section)
build s pos = let 
                    suffix = suffixes !! pos
                    sep = if suffix /= "" then " " else ""
                    (sectionsep, section) = s
              in (sep ++ suffix, sectionsep, section)

padToMultipleof3 :: String -> String
padToMultipleof3 s = (replicate n '0') ++ s 
            where
                d = ((length s) `mod` 3)
                n = if (d == 0) then 0 else (3 - d)

tosectionlist :: String -> [Section]
tosectionlist []  = []
tosectionlist (h:t:u:xs) = (tosection h t u)  : tosectionlist xs

tosection :: Char -> Char -> Char -> Section
tosection h t u = S (tohundreds h) (totensunits t u)


tohundreds :: Char -> HundredsPart 
tohundreds a = if a == '0' then NoHundreds else Hundred a

totensunits :: Char -> Char -> TensUnitsPart
totensunits b c  
    | b == '0' && c == '0' = NoTensUnits
    | b == '0' && c /= '0' = Someteen b c
    | b == '1'             = Someteen b c
    | otherwise            = Somety b c


towords :: Section -> String
towords (S NoHundreds NoTensUnits) = ""
towords (S NoHundreds b) = tensunitstowords b
towords (S a NoTensUnits) = hundredstowords a
towords (S a b) = (hundredstowords a) ++ " and " ++ (tensunitstowords b)

hundredstowords :: HundredsPart -> String
hundredstowords s =
    case s of
        Hundred a   -> (translateunits a) ++ " hundred"
        NoHundreds -> ""

tensunitstowords :: TensUnitsPart -> String
tensunitstowords s =
    case s of
        Somety a b  -> (translatetens a) ++ if (b /= '0') then 
                                                " " ++ (translateunits b)
                                            else
                                                ""
        Someteen a b -> if (a=='0') then 
                            translateunits b
                        else
                            translateteens [a,b]
        NoTensUnits -> ""
          
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
        

-- main :: IO()
-- main = do
--     putStrLn "type in a number"
--     inp <- getLine
--     putStrLn $ (show . toSentance) inp