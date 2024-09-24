{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiParser where

import Control.Arrow ((>>>))
import Control.Monad (forM, void, (<=<))
import Data.Char (digitToInt, isUpper)
import Data.Data (Data, Typeable)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List (foldl', stripPrefix)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (
    Assoc (..),
    Operator (..),
    OperatorTable,
    buildExpressionParser,
 )

intro :: Q [Dec]
intro = return []

data Format
    = Empty
    | Signed
    | Unsigned
    | Char
    | Newline
    | Symbol
    | String
    | Optional !Format
    | At !String
    | Literal !String
    | Gather !Format
    | Group !Format
    | Many !Format
    | Some !Format
    | Alternative !Format !Format
    | SepBy !Format !Format
    | Follows !Format !Format
    deriving (Show, Typeable, Data)

fmt :: QuasiQuoter
fmt =
    QuasiQuoter
        { quoteExp = makeParser <=< parseF
        , quoteType = toType <=< parseF
        , quotePat = const $ fail "Patterns not supported"
        , quoteDec = const $ fail "Decs not supported"
        }
  where
    makeParser p = [|parseErr ($(toParser p) <* eof)|]

toType :: Format -> TypeQ
toType = \case
    Empty -> [t|()|]
    Newline -> [t|()|]
    Unsigned -> [t|Int|]
    Symbol -> [t|Char|]
    Signed -> [t|Int|]
    String -> [t|String|]
    Char -> [t|Char|]
    Literal _ -> [t|()|]
    Optional format
        | interesting format -> [t|Maybe $(toType format)|]
        | otherwise -> [t|()|]
    At ts
        | isUpper (head ts) -> conT (mkName ts)
        | otherwise -> fail "toType: can't read type variables"
    Group format -> [t|$(toType format)|]
    Gather format -> [t|$(toType format)|]
    Many format -> if interesting format then [t|[$(toType format)]|] else [t|()|]
    Some format -> if interesting format then [t|[$(toType format)]|] else [t|()|]
    SepBy format _sep -> if interesting format then [t|[$(toType format)]|] else [t|()|]
    Alternative l r
        | interesting l, interesting r -> [t|Either $(toType l) $(toType r)|]
        | interesting l -> [t|Maybe $lt|]
        | interesting r -> [t|Maybe $rt|]
        | otherwise -> [t|()|]
      where
        lt = toType l
        rt = toType r
    Follows l r -> [t|($(toType l), $(toType r))|]

toParser :: Format -> ExpQ
toParser = \case
    Empty -> [|return ()|]
    Newline -> [|void newline|]
    String -> [|many1 letter|]
    Symbol -> [|many1 symbol|]
    Unsigned -> [|unsigned|]
    Signed -> [|signed|]
    Char -> [|letter|]
    Optional format
        | interesting format -> [|option Nothing (Just <$> $(toParser format))|]
        | otherwise -> [|optional $(toParser format)|]
    Gather format -> [|fst <$> gather $(toParser format)|]
    At tag
        | isUpper (head tag) -> makeEnumParser tag
        | otherwise -> varE (mkName tag)
    Literal str -> [|void (string str)|]
    Group format -> [|$(toParser format)|]
    Many format ->
        if interesting format
            then [|many $(toParser format)|]
            else [|void (many $(toParser format))|]
    Some format ->
        if interesting format
            then [|many1 $(toParser format)|]
            else [|void (many1 $(toParser format))|]
    SepBy l r ->
        if interesting l
            then [|sepBy $(toParser l) $(toParser r)|]
            else [|void (sepBy $(toParser l) $(toParser r))|]
    Alternative l r
        | interesting l, interesting r -> [|Left <$> $le <|> Right <$> $re|]
        | interesting l -> [|Just <$> $le <|> Nothing <$ $re|]
        | interesting r -> [|Nothing <$ $le <|> Just <$> $re|]
        | otherwise -> [|$le <|> $re|]
      where
        le = toParser l
        re = toParser r
    format@(Follows _ _) -> do
        let fmts = [(interesting x, toParser x) | x <- flatten format []]
            n = foldl' (\acc (x, _) -> if x then acc + 1 else acc) 0 fmts
            tup = conE (tupleDataName n)
        case fmts of
            [] -> [|return ()|]
            ((ii, e) : es)
                | n == 0 -> foldl' ap0 e es
                | n == 1 -> foldl' ap1 e es
                | ii -> foldl' apN [|$tup <$> $e|] es
                | otherwise -> foldl' apN [|$tup <$ $e|] es
      where
        ap0 l (_, r) = [|$l *> $r|]
        ap1 l (i, r) = if i then [|$l *> $r|] else [|$l <* $r|]
        apN l (i, r) = if i then [|$l <*> $r|] else [|$l <* $r|]

interesting :: Format -> Bool
interesting = \case
    Empty -> False
    Literal{} -> False
    Newline -> False
    Signed -> True
    Symbol -> True
    Unsigned -> True
    Char -> True
    At _ -> True
    String -> True
    Gather _ -> True
    Optional format -> interesting format
    Group format -> interesting format
    Many format -> interesting format
    Some format -> interesting format
    SepBy l _ -> interesting l
    Alternative l r -> interesting l || interesting r
    Follows l r -> interesting l || interesting r

gather :: Parser a -> Parser (String, a)
gather p = do
    before <- getInput
    res <- p
    afterLen <- length <$> getInput
    let parStr = take (length before - afterLen) before
    return (parStr, res)

symbol :: Parser Char
symbol = oneOf "!@#$%^&*_+=|'\";:"

decimal :: [Int] -> Int
decimal = foldl' (\acc -> ((10 * acc) +)) 0

unsigned :: Parser Int
unsigned = decimal <$> many1 (digitToInt <$> digit)

signed :: Parser Int
signed = do
    f <- option id (char '-' $> negate <|> char '+' $> id)
    f . decimal <$> many1 (digitToInt <$> digit)

flatten :: Format -> [Format] -> [Format]
flatten (Literal x) (Literal y : ys) = flatten (Literal (x ++ y)) ys
flatten Empty xs = xs
flatten (Follows l r) xs = flatten l (flatten r xs)
flatten x ys = x : ys

parseF :: String -> Q Format
parseF s = case parseEither (factor1 <* eof) s of
    Left err -> fail (show err)
    Right r -> return r

makeEnumParser :: String -> ExpQ
makeEnumParser xs = do
    cases <- go xs
    let parsers = [[|$(conE name) <$ string str|] | (name, str) <- cases]
    [|choice $(listE parsers)|]
  where
    go :: String -> Q [(Name, String)]
    go str = do
        tyInfo <- lookupTypeName str
        tyName <- maybe (fail "Data type not found") return tyInfo
        cons <-
            reify tyName >>= \case
                TyConI (DataD _ _ _ _ cons _) -> return cons
                _ -> fail $ "Failed finding data type: " <> str
        forM cons $ \case
            NormalC n _
                | Just name <- stripPrefix str (nameBase n) ->
                    case name of
                        '_' : symbolName -> do
                            sym <- processSymbolName symbolName
                            return (n, sym)
                        _ -> return (n, name)
                | otherwise -> return (n, nameBase n)
            _ -> fail "Not an enum constructor"

processSymbolName :: String -> Q String
processSymbolName str =
    case break ('_' ==) str of
        (name, rest) ->
            case lookup name symbolNames of
                Nothing -> return name
                Just sym ->
                    case rest of
                        [] -> pure [sym]
                        _ : str' -> (sym :) <$> processSymbolName str'

symbolNames :: [(String, Char)]
symbolNames =
    [ ("LT", '<')
    , ("GT", '>')
    , ("EQ", '=')
    , ("BANG", '!')
    , ("AT", '@')
    , ("HASH", '#')
    , ("DOLLAR", '$')
    , ("PERCENT", '%')
    , ("CARET", '^')
    , ("AMPERSAND", '&')
    , ("STAR", '*')
    , ("PIPE", '|')
    , ("LPAREN", '(')
    , ("RPAREN", ')')
    , ("LBRACE", '{')
    , ("RBRACE", '}')
    , ("LBRACK", '[')
    , ("RBRACK", ']')
    , ("COLON", ':')
    , ("SEMI", ';')
    , ("QUESTION", '?')
    , ("SLASH", '/')
    , ("BACKSLASH", '\\')
    , ("UNDERSCORE", '_')
    , ("DASH", '-')
    , ("DOT", '.')
    , ("COMMA", ',')
    , ("PLUS", '+')
    , ("TILDE", '~')
    ]

-- Parsing for the Quasi Quoter

type Parser = Parsec String ()
type Table a = OperatorTable String () Identity a

parseEither :: Parser a -> String -> Either ParseError a
parseEither p = parse (p <* eof) ""

parseErr :: Parser a -> String -> a
parseErr p s = case parseEither p s of
    Left err -> error (show err)
    Right r -> r

factor1 :: Parser Format
factor1 =
    choice
        [ try $ factor2 `chainl1` (char '|' $> Alternative)
        , factor2
        ]
factor2 :: Parser Format
factor2 =
    choice
        [ try $ factor3 `chainl1` (return () $> Follows)
        , factor3
        ]

factor3 :: Parser Format
factor3 =
    choice
        [ try $ buildExpressionParser table atom
        , atom
        ]

atom :: Parser Format
atom =
    choice
        [ try $ Group <$> between (char '(') (char ')') factor1
        , try $
            foldl1 Alternative
                <$> between
                    (char '[')
                    (char ']')
                    (many1 (Literal <$> fmap (: []) (chars <|> char '\\' *> anyChar)))
        , try $ string "%s" $> String
        , try $ string "%d" $> Signed
        , try $ string "%c" $> Char
        , try $ string "%n" $> Newline
        , try $ string "%u" $> Unsigned
        , try $ string "%y" $> Symbol
        , try $ string "@" *> (At <$> many1 letter)
        , Literal <$> (char '\\' *> fmap (: []) anyChar)
        , Literal <$> many1 chars
        ]
chars :: Parser Char
chars = noneOf "%()\\*+&|@!?[]"

table :: Table Format
table =
    [
        [ Postfix $
            foldr1 (>>>)
                <$> many1
                    ( choice
                        [ char '!' $> Gather
                        , char '?' $> Optional
                        , char '*' $> Many
                        , char '+' $> Some
                        ]
                    )
        ]
    ,
        [ Infix (char '&' $> SepBy) AssocLeft
        ]
    ]
