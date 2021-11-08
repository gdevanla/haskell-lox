{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module LispParser where

import Import hiding (many, (<|>), try)
import Data.Text as T
import Data.Char
import Text.Parsec.Char as PC
import Text.Parsec
import Text.Parsec.String as PS
import qualified Data.List as L
import Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Except

data LispToken =
  LParen
  | RParen
  | Symbol T.Text
  | String T.Text
  | Numeric T.Text
  | Plus
  | Mult
  | Sub
  deriving (Eq, Show)

-- data LispTokInfo = LispTokInfo LispToken SourcePos

type Lexer = Either ParseError [LispToken]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

charMapping :: [(LispToken, Char)]
charMapping =
  [ (LParen, '('),
    (RParen, ')')
  ]

scanSingleCharToken :: Parser LispToken
scanSingleCharToken = choice $ build <$> charMapping
  where
    build :: (LispToken, Char) -> Parser LispToken
    build (x, y) = x <$ char y <* whitespace


scanPrimitive :: Parser LispToken
scanPrimitive = do
  prim <- oneOf "+-*" <* whitespace
  case prim of
    '+' -> return Plus
    '-' -> return Sub
    '*' -> return Mult
    _ -> error "Error incorrect parser set in scanPrimitive"

scanSymbol :: Parser LispToken
scanSymbol = do
  fc <- firstChar
  rest <- many nonFirstChar
  void whitespace
  return $ Symbol $ T.pack (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_' || a == '-')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

scanNumeric :: Parser LispToken
scanNumeric = Numeric . T.pack <$> Text.Parsec.many1 digit <* whitespace


lexTokens :: Parser LispToken
lexTokens = try lParens <|> try rParens <|> try scanPrimitive <|> try scanSymbol <|> scanNumeric
  -- l <- lParens
  -- toks <- try scanSymbolL <|> try scanNumberL <|> try rParens <|> lexTokens
  -- return $ [l] ++ toks
  where
    lParens = LParen <$ satisfy (== '(') <* whitespace
    rParens = RParen <$ satisfy (== ')') <* whitespace

lexer :: String -> Lexer
lexer = parse (many lexTokens) ""


-- Parser functions

newtype Identifier = Identifier {unIdent:: T.Text}
  deriving (Eq, Show)

data Expr
  = ExprLitNum Int
  | ExprVar T.Text
  | ExprLambda [Identifier] Expr
  | ExprApp Expr [Expr]
  | ExprIf !Expr !Expr !Expr
  | ExprPrim !Primitive [Expr]
  deriving (Eq, Show)

data Primitive = PrimAdd | PrimSub | PrimMult
  deriving (Eq, Show)

type ParserResult = Either ParseError Expr
type LispParser a = ParsecT [LispToken] () Identity a

satisfyTok :: (LispToken -> Maybe a) -> LispParser a
satisfyTok f = tokenPrim showTok updateTokPos match
  where
    showTok ti = show ti
    updateTokPos pos _ _ = pos
    match t = f t

satisfyLParen :: LispToken -> Maybe Bool
satisfyLParen LParen = Just True
satisfyLParen _ = Nothing

satisfyRParen :: LispToken -> Maybe Bool
satisfyRParen RParen = Just True
satisfyRParen _ = Nothing
satisfySymbol :: LispToken -> Maybe Expr
satisfySymbol (Symbol s) = Just $ ExprVar s
satisfySymbol _ = Nothing

satisfyIdentifier :: LispToken -> Maybe Identifier
satisfyIdentifier (Symbol s) = Just $ Identifier s
satisfyIdentifier _ = Nothing

satisfyNumeric :: LispToken -> Maybe Expr
satisfyNumeric (Numeric s) = ExprLitNum <$> (readMaybe (T.unpack s)::Maybe Int)
satisfyNumeric _ = Nothing

satisfyLambda :: LispToken -> Maybe Bool
satisfyLambda (Symbol s) = Just $ s == "lambda"
satisfyLambda _ = Nothing

satisfyIf :: LispToken -> Maybe Bool
satisfyIf (Symbol s) = Just $ s == "if"
satisfyIf _ = Nothing

satisfyPrimitive :: LispToken -> Maybe Primitive
satisfyPrimitive Plus = Just PrimAdd
satisfyPrimitive Sub = Just PrimSub
satisfyPrimitive Mult = Just PrimMult
satisfyPrimitive _ = Nothing

exprVar :: LispParser Expr
exprVar = satisfyTok satisfySymbol

exprLambda :: ParsecT [LispToken] () Identity Expr
exprLambda = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyLambda
  void $ satisfyTok satisfyLParen
  ids <- many $ satisfyTok satisfyIdentifier
  void $ satisfyTok satisfyRParen
  expr <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprLambda ids expr

exprApp :: ParsecT [LispToken] () Identity Expr
exprApp = do
  void $ satisfyTok satisfyLParen
  rator <- exprExpr
  expressions <- many exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprApp rator expressions

exprPrimitive :: ParsecT [LispToken] () Identity Expr
exprPrimitive = do
  void $ satisfyTok satisfyLParen
  rator <- satisfyTok satisfyPrimitive
  expressions <- many exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprPrim rator expressions

exprIf :: ParsecT [LispToken] () Identity Expr
exprIf = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyIf
  test_exp <- exprExpr
  true_exp <- exprExpr
  false_exp <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprIf test_exp true_exp false_exp

exprExpr :: ParsecT [LispToken] () Identity Expr
exprExpr = do
  try exprLambda
    <|> try exprPrimitive
    <|> try exprIf
    <|> try exprApp
    <|> try (satisfyTok satisfyNumeric)
    <|> satisfyTok satisfySymbol

printExpr :: Expr -> Int -> T.Text
printExpr (ExprLitNum x) _indent = T.pack . show $ x
printExpr (ExprVar x) _indent = x
printExpr (ExprLambda ids e) indent = let
  ids' = T.intercalate " " $ L.map unIdent ids
  x = T.pack "(lambda (" <> ids' <> ")"
  y = printExpr e (indent + 2)
  in
    x <> "\n" <> T.replicate (indent + 2) " " <> y <> ")"
printExpr (ExprApp rator rands) indent =
  let rands' = (flip printExpr indent <$> rands)
      rands'' = T.intercalate (T.pack " ") rands'
      rands''' = if L.null rands' then "" else " " <> rands''
      new_line = case rator of
        ExprLambda _ _ -> "\n" <> T.replicate indent " "
        _ -> ""
   in T.pack "(" <> printExpr rator indent <> new_line <> rands''' <> ")"
printExpr (ExprPrim rator rands) indent =
  let rands' = (flip printExpr indent <$> rands)
      rands'' = T.intercalate (T.pack " ") rands'
      rands''' = if L.null rands' then "" else " " <> rands''
      new_line = T.replicate indent " "
      op = case rator of
        PrimAdd -> T.pack "+"
        PrimSub -> T.pack "-"
        PrimMult -> T.pack "*"
   in T.pack "(" <> op <> new_line <> rands''' <> ")"
printExpr (ExprIf test_exp true_exp false_exp) indent = let
  test_exp' = printExpr test_exp (indent + 5)
  true_exp' = printExpr true_exp (indent + 5)
  false_exp' = printExpr false_exp (indent + 5)
  indents = T.replicate (indent + 4) " "
  in
    T.pack "(if " <> test_exp' <> "\n" <> indents <> true_exp' <> "\n" <> indents <> false_exp' <> ")"

--printExpr x _ = error $ show $ "not implemented for " ++ show x

parseExpr :: LispParser Expr
parseExpr = exprExpr

lexAndParse :: String -> ParserResult
lexAndParse s = case lexer s of
  Right toks -> parse parseExpr "" toks
  Left e -> Left e

-- Interpreter

data LispValue = LispInt Int
  | LispIdentifier T.Text
  deriving (Show, Eq)

data Env = Env {
               env :: !(M.Map T.Text LispValue),
               parent :: !(Maybe Env)
               } deriving (Show, Eq)

data LispError = SystemError !T.Text
  | ControlFlow !LispValue deriving (Show, Eq)

type InterpreterTIO = ExceptT LispError (StateT Env IO) LispValue

initEnv :: Maybe Env -> Env
initEnv !parent = Env {env=M.empty, parent=parent}
--{-# INLINE initEnv #-}

lookupEnv :: T.Text -> Env -> Maybe LispValue
lookupEnv !k !environ = go (Just environ)
  where
    go (Just Env{..}) = case M.lookup k env of
                           Just v -> Just $! v
                           Nothing -> go parent
    go Nothing = Nothing
--{-# INLINE lookupEnv #-}

updateEnv :: T.Text -> LispValue -> Env -> Maybe Env
updateEnv !k !v !s = go (Just $! s)
  where
    go (Just s'@Env{..}) = case M.lookup k env of
      Just _ -> Just s'{env=M.update (\_ -> Just v) k env}
      Nothing -> do
        !parent_state <- go parent
        return $ s'{parent=Just parent_state}
    go Nothing = Nothing

insertEnv :: T.Text -> LispValue -> Env -> Env
insertEnv !k !v s@Env {..} = s {env = M.insert k v env}
--{-# INLINE insertEnv #-}

multiInsertEnv :: [(T.Text, LispValue)] -> Env -> Env
multiInsertEnv !values s@Env {..} = let
  new_env = M.fromList values
  !new_env' = M.union new_env env
  in
  s {env=new_env'}
-- {-# INLINE multiInsertEnv #-}

unpackIdent :: LispValue -> InterpreterTIO
unpackIdent (LispIdentifier x) = do
  s <- get
  let v1 = lookupEnv x s
  case v1 of
    Just v' -> lift . return $ v'
    Nothing -> ExceptT . return $ Left $ SystemError $ "Unknown var: " <> x
unpackIdent x = lift . return $ x

--{-# INLINE unpackIdent #-}

interpretExpr :: Expr -> InterpreterTIO
interpretExpr (ExprLitNum a) = return $ LispInt a
interpretExpr (ExprVar var) = undefined
interpretExpr (ExprLambda ids expr) = undefined
interpretExpr (ExprApp exps [exprs]) = undefined
interpretExpr (ExprIf test_exp true_exp false_exp) = undefined
interpretExpr (ExprPrim prim exprs) = do
  rands <- mapM interpretExpr exprs
  let rands' = traverse convert rands
  case rands' of
    Right vals -> return $ LispInt $ applyPrim (func prim) vals
    Left e -> ExceptT . return . Left $ SystemError e
  where
    convert (LispInt a) = Right a
    convert x = Left $ T.pack "Invalid rand for primitive type: " <> T.pack (show x)

    applyPrim :: (Int -> Int -> Int) -> [Int] -> Int
    applyPrim func' vs = L.foldl' func' 0 vs

    func PrimAdd = (+)
    func PrimSub = (-)
    func PrimMult = (*)

  -- case prim of
  --   PrimAdd -> applyPrim (+) rands
  -- where
  --   applyPrim' :: Num a=> (a -> a -> a) -> [LispValue] -> [LispValue]
  --   applyPrim' func vals = return $ LispInt $ L.foldl' (step func) (LispValue 0) vals

  --   step :: (Int->Int->Int) -> LispValue -> LispValue -> LispValue
  --   step func acc rand = case (acc, rand) of
  --     (LispInt acc', LispInt rand') -> LispInt $ func acc' rand'
  --     _ -> Left "Invalid type for primtivies"


runInterpreter :: String -> IO (Either LispError LispValue)
runInterpreter input = do
  let result = lexAndParse input
  case result of
    Right r -> do
      let inter = runExceptT (interpretExpr r)
      (final, _) <- runStateT inter (initEnv Nothing)
      case final of
        Right f -> return $ Right f
        Left e -> return $ Left e
    Left e -> error $ show e
