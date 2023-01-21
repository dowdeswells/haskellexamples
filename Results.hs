module Results
where


data Result a =
    Ok a
    | Error [String]
    deriving Show

instance Functor Result where
    fmap _ (Error ms) = Error ms
    fmap g (Ok x)     = Ok (g x)

instance Applicative Result where
    pure = Ok
    (Error ms1) <*> (Error ms2) = Error $ ms1 ++ ms2
    (Error ms1) <*> (Ok _) = (Error ms1)
    (Ok _) <*> (Error ms2) = (Error ms2)
    (Ok g) <*> mx    = fmap g mx

instance Monad Result where
    (Error ms) >>= _ = (Error ms)
    (Ok x) >>= f = f x



data SearchCriteria = SearchCriteria {
    nameCriteria            :: String,
    anotherNameCriteria     :: String,
    pageNo                  :: Int,
    pageSize                :: Int
} deriving Show


toValidator :: [Char] -> (a -> Bool) -> [Char] -> (p -> a) -> p -> Result a
toValidator message f fieldName getter = 
        \input -> 
                let v = getter input 
                in case f v of
                        True -> Ok v
                        False -> Error [fieldName ++ ": " ++ message]

isLetter :: Char -> Bool
isLetter character = if character `elem` ['a'..'z'] || character `elem` ['A'..'Z']
                     then True
                     else False


areLetters :: String -> Bool
areLetters = 
    foldl (\s c -> s && isLetter c) True

isInRange :: Ord a => a -> a -> a -> Bool
isInRange min max a = 
    a >= min && a <= max


validateJustLetters = toValidator  "Must be just letters" areLetters
validateIsInRange min max = 
    toValidator message f 
    where
        f = isInRange min max
        message = "Must be between " ++ show(min) ++ " and " ++ show(max)  

validateNameCriteria = validateJustLetters "nameCriteria" nameCriteria 
validateAnotherNameCriteria = validateJustLetters "anotherNameCriteria" anotherNameCriteria 
validatePageNo = validateIsInRange 1 10000000 "pageNo" pageNo
validatePageSize = validateIsInRange 1 500 "pageSize" pageSize


validateSearchCriteria sc =
    validateNameCriteria sc >>= \name ->
    validateAnotherNameCriteria sc >>= \anotherName ->
    validatePageNo sc >>= \pageNo ->
    validatePageSize sc >>= \pageSize ->
    Ok (SearchCriteria name anotherName pageNo pageSize)

doValidateSearchCriteria sc = do 
    name <- validateNameCriteria sc
    anotherName <- validateAnotherNameCriteria sc
    pageNo <- validatePageNo sc
    pageSize <- validatePageSize sc
    return (SearchCriteria name anotherName pageNo pageSize)


applyValidate sc =
    SearchCriteria
    <$> validateNameCriteria sc 
    <*> validateAnotherNameCriteria sc
    <*> validatePageNo sc 
    <*> validatePageSize sc 



allbadCriteria = SearchCriteria {nameCriteria="simon99", anotherNameCriteria="dow88", pageNo=0, pageSize=60000}
somebadCriteria = SearchCriteria {nameCriteria="simon99", anotherNameCriteria="dow", pageNo=1, pageSize=60000}
goodCriteria = SearchCriteria {nameCriteria="simon", anotherNameCriteria="dow", pageNo=100, pageSize=60}
