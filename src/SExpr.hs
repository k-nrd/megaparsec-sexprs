module SExpr where

import Data.Scientific (floatingOrInteger)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, between, choice, errorBundlePretty, label, many, optional, parse, takeWhileP, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, char', letterChar, space1, string)
import Text.Megaparsec.Char.Lexer (lexeme, scientific, signed, skipBlockCommentNested, skipLineComment, space)

type Parser = Parsec Void Text

newtype Identifier = Identifier {getId :: Text}
  deriving stock (Show)

data Atom
  = SInteger Integer -- 42
  | SDouble Double -- 42f, 3.1415f
  | SString Text -- "hello, world"
  | SBool Bool -- false, true
  | SId Identifier -- foo
  deriving stock (Show)

data Expr
  = SExpr Identifier [Expr]
  | SAtom Atom
  deriving stock (Show)

-- <$ operator replaces a value in a functor with the value on the left hand side
bool :: Parser Bool
bool =
  label "boolean" $
    choice
      [ fmap (const False) (string "false"),
        fmap (const True) (string "true")
      ]

str :: Parser Text
str =
  label "string" $
    between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

identifier :: Parser Identifier
identifier = label "identifier" $
  do
    first <- letterChar <|> char '_'
    rest <- many $ alphaNumChar <|> char '_'
    return $ Identifier (pack (first : rest))

numeric :: Parser Atom
numeric =
  label "number" $ do
    value <- signed skipSpace scientific
    case floatingOrInteger value of
      Left d -> SDouble d <$ char' 'f'
      Right i -> do
        f <- optional $ char' 'f'
        return $ case f of
          Nothing -> SInteger i
          Just _ -> SDouble $ fromIntegral i

skipSpace :: Parser ()
skipSpace =
  space
    space1
    (skipLineComment ";;")
    (skipBlockCommentNested "/*" "*/")

lxm :: Parser a -> Parser a
lxm = lexeme skipSpace

atom :: Parser Expr
atom =
  label "atom" $
    SAtom
      <$> choice
        [ fmap SBool bool,
          numeric,
          fmap SString str,
          fmap SId identifier
        ]

sexpr :: Parser Expr
sexpr =
  label "S-expression" $
    between (lxm $ char '(') (char ')') (SExpr <$> lxm identifier <*> many expr)

expr :: Parser Expr
expr =
  label "expression" $
    lxm $
      sexpr <|> atom

parseExpr :: Text -> Either String Expr
parseExpr input =
  let outputE = parse (between skipSpace eof expr) "" input
   in case outputE of
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output
