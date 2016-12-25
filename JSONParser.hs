module JSON (JSONVal (..), readJSON) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Arrow
import Parser --TODO -- make a copy of my parser in here
import Control.Monad.Fail
import Prelude hiding (fail)
import Data.Char (isHexDigit, isDigit, digitToInt, chr, ord)

data JSONVal = JSONNull | JSONBool Bool | JSONString String | JSONNum Double | JSONArray [JSONVal] | JSONObj (Map String JSONVal)

--Implemented based on the specs found at: http://rfc7159.net/rfc7159

instance Show JSONVal where
    show (JSONNull) = "null"
    show (JSONBool True) = "true"
    show (JSONBool False) = "false"
    show (JSONNum x) = show x
    show (JSONString str) = show str
    show (JSONArray l) = show l --Haskell gives us the [] for free.
    show (JSONObj o) = if Map.null o then "{}" else '{': (foldl1 (\acc e -> acc ++ ',':e) . map (\e -> (show . fst) e ++ ':':(show . snd) e)) (Map.toList o) ++ "}"

readJSON :: String -> Maybe JSONVal
readJSON = getResult . tryParse parseJSON

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
        isWhiteSpace = flip elem [' ', '\t', '\n', '\r']
        
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
    parseValue = parseNull <||> parseBool <||> parseString <||> parseNum <||> parseArray <||> parseObject
    
    parseNull = parseWord "null" >> return JSONNull
    parseBool = parseTrue <||> parseFalse where
        
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
                        '\"' -> return '\"'; '\\' -> return '\\'; '/' -> return '/'
                        'r' -> return '\r'; 'f' -> return '\f'; 'n' -> return '\n'; 'b' -> return '\b'; 't' -> return '\t'
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
            return (0x1000*a + 0x100*b + 0x10*c + d)
          parseHexDigit = do
            a <- parseAnyChar
            if isHexDigit a then return (digitToInt a) else fail [a]
        
    parseNum = fmap process $ optional (parseChar '-') <&> parseInt <&> optional parseFrac <&> optional parseExp where
        
          parseInt = fmap (either (const "0") (uncurry (:))) (parseChar '0' <|> parseNonzeroDigit <&> kleeneStar parseDigit)
          parseFrac = do 
            dot <- parseChar '.'
            digit <- parseDigit 
            digits <- kleeneStar parseDigit
            return $ dot:digit:digits
          parseDigit = parseAnyChar >>= \c -> if isDigit c then return c else fail [c]
          parseNonzeroDigit = parseDigit >>= \c -> if c == '0' then fail [c] else return c
          parseExp = do
            e <- parseChar 'e' <||> parseChar 'E'
            mbSign <- optional (parseChar '-' <||> parseChar '+')
            (dig, digs) <- parseDigit <&> kleeneStar parseDigit
            return $ case mbSign of
                        Nothing -> e : dig : digs
                        Just s -> e : s : dig : digs
          process (((mbMinus, int), mbFrac), mbExp) = JSONNum $ read (showMb mbMinus ++ int ++ showMb mbFrac ++ showMb mbExp) where showMb Nothing = ""; showMb (Just x) = show x --Use the built-in read function to parse it as a Double.
              
    parseArray = do
        begin_array
        l <- optional parseInner 
        end_array
        return $ case l of Nothing -> JSONArray []; Just x -> JSONArray x; where
            
          parseInner :: Parser String [JSONVal]
          parseInner = fmap (uncurry (:)) $ parseValue <&> kleeneStar (value_separator >> parseValue)
