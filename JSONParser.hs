module JSON (JSONVal (..), readJSON, test) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Arrow
import Parser --TODO -- make a copy of my parser in here
import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Char (isHexDigit, digitToInt, chr, ord)

data JSONVal = JSONNull | JSONBool Bool | JSONString String | JSONNum Double | JSONArray [JSONVal] | JSONObj (Map String JSONVal) deriving Show

readJSON :: String -> JSONVal
readJSON json = case tryParse parseJSON json of
                  Just ([], x) -> x
                  otherwise -> error "Invalid JSON text."

parseJSON :: Parser String JSONVal
parseJSON = do
    wsParse
    out <- parseValue
    wsParse
    return out where
    
    wsParse :: Parser String [Char]
    wsParse = kleeneStar singlewsParse where
        
        singlewsParse = do 
            a <- parseAnyChar
            if isWhiteSpace a then return a else fail [a]
        isWhiteSpace c = case c of
                            ' ' -> True
                            '\t' -> True
                            '\n' -> True
                            '\r' -> True
                            otherwise -> False
        
    wsParseChar :: Char -> Parser String Char
    wsParseChar char = do
        wsParse
        a <- parseChar char
        wsParse
        return a
            
    begin_array = wsParseChar '['
    begin_object = wsParseChar '{'
    end_array = wsParseChar ']'
    end_object = wsParseChar '}'
    name_separator = wsParseChar ':'
    value_separator = wsParseChar ','
    
    parseValue :: Parser String JSONVal
    parseValue = flip fmap (parseNull <|> parseBool <|> parseString <|> parseNum <|> parseArray <|> parseObject) (\res -> case res of
        Left x -> case x of
                       Left y -> case y of
                                      Left z -> case z of
                                                     Left a -> case a of
                                                                    Left b -> b
                                                                    Right b -> case b of
                                                                                    Left c -> c
                                                                                    Right c -> c
                                                     Right a -> a
                                      Right z -> z
                       Right y -> y
        Right x -> x)    
    
    parseNull = parseWord "null" >> return JSONNull
    parseBool = fmap (id +++ id) (parseTrue <|> parseFalse) where
        
          parseTrue = parseWord "true" >> return (JSONBool True)
          parseFalse = parseWord "false" >> return (JSONBool False)
    
    parseObject :: Parser String JSONVal
    parseObject = do
        begin_object
        contents <- optional $ parseMember <&> kleeneStar (value_separator >> parseMember)
        end_object
        return $ JSONObj (process contents) where
          process Nothing = Map.empty
          process (Just (mem, mems)) = uncurry Map.insert mem (Map.fromList mems)
            
          parseMember :: Parser String (String, JSONVal)
          parseMember = do
            (JSONString name) <- parseString
            name_separator
            value <- parseValue
            return (name, value)
    
    parseString :: Parser String JSONVal
    parseString = do
        parseChar '\"'
        inside <- kleeneStar parseMaybeEscapedChar
        parseChar '\"'
        return $ JSONString inside where
            
          parseMaybeEscapedChar = do
            a <- parseAnyChar
            let val = ord a
            if val == 0x5C 
                then do
                    b <- parseAnyChar
                    case b of
                        '\"' -> return '\"'
                        '\\' -> return '\\'
                        '/' -> return '/'
                        'r' -> return '\r'
                        'f' -> return '\f'
                        'n' -> return '\n'
                        'b' -> return '\b'
                        't' -> return '\t'
                        'u' -> fmap chr fourHexDig >>= return
                        otherwise -> fail [b]
                else if val == 0x20 || val == 0x21 || val >= 0x23 && val <= 0x5B || val >= 0x5D 
                then return a
                else fail [a]
          fourHexDig = do
            a <- parseHexDigit
            b <- parseHexDigit
            c <- parseHexDigit
            d <- parseHexDigit
            return (16*16*16*a + 16*16*b + 16*c + d)
          parseHexDigit = do
            a <- parseAnyChar
            if isHexDigit a then return (digitToInt a) else fail [a]
        
    parseNum = fmap process $ optional (parseChar '-') <&> parseInt <&> optional parseFrac <&> optional parseExp where
        
          parseInt = flip fmap (parseChar '0' <|> parseNonzeroDigit <&> kleeneStar parseDigit) (\res -> case res of
            Left _ -> ['0']
            Right (x, xs) -> x:xs)
          parseFrac = do 
            dot <- parseChar '.'
            digit <- parseDigit 
            digits <- kleeneStar parseDigit
            return $ dot:digit:digits
          parseDigit = parseAnyChar >>= \c -> if c `elem` ['0','1','2','3','4','5','6','7','8','9'] then return c else fail [c]
          parseNonzeroDigit = parseDigit >>= \c -> if c == '0' then fail [c] else return c
          parseExp = do
            e <- fmap unify $ parseChar 'e' <|> parseChar 'E'
            mbSign <- optional (fmap unify $ parseChar '-' <|> parseChar '+')
            (dig, digs) <- parseDigit <&> kleeneStar parseDigit
            return $ case mbSign of
                        Nothing -> e : dig : digs
                        Just s -> e : s : dig : digs
            where unify = either id id
          process (((mbMinus, int), mbFrac), mbExp) = JSONNum $ read (showMb mbMinus ++ int ++ showMb mbFrac ++ showMb mbExp) where showMb Nothing = ""; showMb (Just x) = show x
              
    parseArray = do
        begin_array
        l <- optional parseInner 
        end_array
        return $ case l of Nothing -> JSONArray []; Just x -> JSONArray x; where
            
          parseInner :: Parser String [JSONVal]
          parseInner = fmap (uncurry (:)) $ parseValue <&> kleeneStar (value_separator >> parseValue)
          
          
          
parseString :: Parser String JSONVal
parseString = do
    parseChar '\"'
    inside <- kleeneStar parseMaybeEscapedChar
    parseChar '\"'
    return $ JSONString inside where
        
        parseMaybeEscapedChar = do
          a <- parseAnyChar
          let val = ord a
          if val == 0x5C 
             then do
                b <- parseAnyChar
                case b of
                    '\"' -> return '\"'
                    '\\' -> return '\\'
                    '/' -> return '/'
                    'r' -> return '\r'
                    'f' -> return '\f'
                    'n' -> return '\n'
                    'b' -> return '\b'
                    't' -> return '\t'
                    'u' -> fmap chr fourHexDig >>= return
                    otherwise -> fail [b]
             else if val == 0x20 || val == 0x21 || val >= 0x23 && val <= 0x5B || val >= 0x5D 
             then return a
             else fail [a]
        
        fourHexDig = do
            a <- parseHexDigit
            b <- parseHexDigit
            c <- parseHexDigit
            d <- parseHexDigit
            return (16*16*16*a + 16*16*b + 16*c + d)
        parseHexDigit = do
            a <- parseAnyChar
            if isHexDigit a then return (digitToInt a) else fail [a]
