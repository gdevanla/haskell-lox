{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LispParser where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Text as T
import Import hiding (many, try, (<|>))
import Text.Parsec
import Text.Parsec.Char as PC
import Text.Parsec.String as PS
import System.IO

data LispToken
  = LParen
  | RParen
  | Symbol T.Text
  | String T.Text
  | Numeric T.Text
  | Plus
  | Mult
  | Sub
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | And
  | Or
  | Not
  | Let
  | Equal
  | In
  | LetRecAnd
  | LetRec
  | Try
  | Catch
  | Raise
  deriving (Eq, Show)

-- data LispTokInfo = LispTokInfo LispToken SourcePos

type Lexer = Either ParseError [LispToken]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

charMapping :: [(LispToken, Char)]
charMapping =
  [ --(LParen, '('),
    --(RParen, ')'),
    (Equal, '=')
  ]

scanSingleCharToken :: Parser LispToken
scanSingleCharToken = choice $ build <$> charMapping
  where
    build :: (LispToken, Char) -> Parser LispToken
    build (x, y) = x <$ char y <* whitespace

scanPrimitiveMultiple :: Parser LispToken
scanPrimitiveMultiple = do
  prim <-
    ( try (string "&&")
        <|> try (string "||")
        <|> try (string ">=")
        <|> try (string "<=")
        <|> try (string ">")
        <|> try (string "<")
        <|> string "=="
      )
      <* whitespace
  case prim of
    "&&" -> return And
    "||" -> return Or
    "not" -> return Not
    ">=" -> return Gte
    "<=" -> return Lte
    ">" -> return Gt
    "<" -> return Lt
    "==" -> return Eq
    _ -> error "Error incorrect parser set in scanPrimitive"

scanPrimitive :: Parser LispToken
scanPrimitive = do
  prim <- oneOf "+-*" <* whitespace
  case prim of
    '+' -> return Plus
    '-' -> return Sub
    '*' -> return Mult
    _ -> error "Error incorrect parser set in scanPrimitive"

scanKeywords :: Parser LispToken
scanKeywords =  try ((return LetRec) <* (string "letrec" <* whitespace)) <|>
                try ((return Let) <* (string "let" <* whitespace)) <|>
                try ((return In) <* (string "in" <* whitespace)) <|>
                try ((return Try) <* (string "try" <* whitespace)) <|>
                try ((return Catch) <* (string "catch" <* whitespace)) <|>
                try ((return Raise) <* (string "raise" <* whitespace)) <|>
                ((return LetRecAnd) <* (string "and" <* whitespace))

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
lexTokens = try lParens <|> try rParens <|> try scanPrimitiveMultiple <|> try scanPrimitive <|>
  try scanSingleCharToken <|> try scanKeywords <|> try scanSymbol <|> scanNumeric
  where
    -- l <- lParens
    -- toks <- try scanSymbolL <|> try scanNumberL <|> try rParens <|> lexTokens
    -- return $ [l] ++ toks

    lParens = LParen <$ satisfy (== '(') <* whitespace
    rParens = RParen <$ satisfy (== ')') <* whitespace

lexer :: String -> Lexer
lexer = parse (many lexTokens) ""

-- Parser functions

newtype Identifier = Identifier {unIdent :: T.Text}
  deriving (Eq, Show)

data Expr
  = ExprLitNum Int
  | ExprVar T.Text
  | ExprLambda [Identifier] Expr
  | ExprApp Expr [Expr]
  | ExprIf !Expr !Expr !Expr
  | ExprPrim !Primitive [Expr]
  | ExprPrimPred !PrimitivePred Expr Expr
  | ExprLet (Identifier, Expr) Expr
  | ExprLetRec [(Identifier, Expr)] Expr
  | ExprTryCatch !Expr !Expr
  | ExprRaise Expr
  deriving (Eq, Show)

data Primitive
  = PrimAdd
  | PrimSub
  | PrimMult
  deriving (Eq, Show)

data PrimitivePred
  = PrimAnd
  | PrimOr
  | PrimEq
  | PrimLt
  | PrimLte
  | PrimGt
  | PrimGte
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
satisfyNumeric (Numeric s) = ExprLitNum <$> (readMaybe (T.unpack s) :: Maybe Int)
satisfyNumeric _ = Nothing

satisfyLambda :: LispToken -> Maybe Bool
satisfyLambda (Symbol s) = if s == "lambda" then Just True else Nothing
satisfyLambda _ = Nothing

satisfyIf :: LispToken -> Maybe Bool
satisfyIf (Symbol s) = if s == "if" then Just True else Nothing
satisfyIf _ = Nothing

satisfyPrimitive :: LispToken -> Maybe Primitive
satisfyPrimitive Plus = Just PrimAdd
satisfyPrimitive Sub = Just PrimSub
satisfyPrimitive Mult = Just PrimMult
satisfyPrimitive _ = Nothing

satisfyPrimitivePredicate :: LispToken -> Maybe PrimitivePred
satisfyPrimitivePredicate And = Just PrimAnd
satisfyPrimitivePredicate Or = Just PrimOr
--satisfyPrimitivePredicate Not = Just PrimNot
satisfyPrimitivePredicate Lt = Just PrimLt
satisfyPrimitivePredicate Lte = Just PrimLte
satisfyPrimitivePredicate Gt = Just PrimGt
satisfyPrimitivePredicate Gte = Just PrimGte
satisfyPrimitivePredicate Eq = Just PrimEq
satisfyPrimitivePredicate _ = Nothing

satisfyLet :: LispToken -> Maybe Bool
satisfyLet Let = Just True
satisfyLet _ = Nothing

satisfyLetRec :: LispToken -> Maybe Bool
satisfyLetRec LetRec = Just True
satisfyLetRec _ = Nothing

satisfyIn :: LispToken -> Maybe Bool
satisfyIn In = Just True
satisfyIn _ = Nothing

satisfyEqual :: LispToken -> Maybe Bool
satisfyEqual Equal = Just True
satisfyEqual _ = Nothing

satisfyLetRecAnd :: LispToken -> Maybe Bool
satisfyLetRecAnd LetRecAnd = Just True
satisfyLetRecAnd _ = Nothing

satisfyTry :: LispToken -> Maybe Bool
satisfyTry Try = Just True
satisfyTry _ = Nothing

satisfyCatch :: LispToken -> Maybe Bool
satisfyCatch Catch = Just True
satisfyCatch _ = Nothing

satisfyRaise :: LispToken -> Maybe Bool
satisfyRaise Raise = Just True
satisfyRaise _ = Nothing

exprVar :: LispParser Expr
exprVar = satisfyTok satisfySymbol

exprTryCatch :: ParsecT [LispToken] () Identity Expr
exprTryCatch = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyTry
  try_expr <- exprExpr
  void $ satisfyTok satisfyCatch
  catch_expr <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprTryCatch try_expr catch_expr

exprRaise :: ParsecT [LispToken] () Identity Expr
exprRaise = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyRaise
  raise_expr <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprRaise raise_expr

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
  expressions <- many1 exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprApp rator expressions

exprPrimitive :: ParsecT [LispToken] () Identity Expr
exprPrimitive = do
  void $ satisfyTok satisfyLParen
  rator <- satisfyTok satisfyPrimitive
  expressions <- many exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprPrim rator expressions

exprPrimitivePredicate :: ParsecT [LispToken] () Identity Expr
exprPrimitivePredicate = do
  void $ satisfyTok satisfyLParen
  rator <- satisfyTok satisfyPrimitivePredicate
  expr1 <- exprExpr
  expr2 <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprPrimPred rator expr1 expr2

exprIf :: ParsecT [LispToken] () Identity Expr
exprIf = do
  void $ satisfyTok satisfyLParen
  void $ satisfyTok satisfyIf
  test_exp <- exprExpr
  true_exp <- exprExpr
  false_exp <- exprExpr
  void $ satisfyTok satisfyRParen
  return $ ExprIf test_exp true_exp false_exp


exprLetBinding :: ParsecT [LispToken] () Identity (Identifier, Expr)
exprLetBinding = do
  symbol <- satisfyTok satisfyIdentifier
  void $ satisfyTok satisfyEqual
  expr <- exprExpr
  return (symbol, expr)

exprLetBindings :: ParsecT [LispToken] () Identity (Identifier, Expr)
exprLetBindings = exprLetBinding

exprLetRecBindings :: ParsecT [LispToken] () Identity [(Identifier, Expr)]
exprLetRecBindings = sepBy exprLetBinding (satisfyTok satisfyLetRecAnd)

exprLet :: ParsecT [LispToken] () Identity Expr
exprLet = do
  void $ satisfyTok satisfyLet
  bindings <- exprLetBindings
  void $ satisfyTok satisfyIn
  expr <- exprExpr
  return $ ExprLet bindings expr

exprLetRec :: ParsecT [LispToken] () Identity Expr
exprLetRec = do
  void $ satisfyTok satisfyLetRec
  bindings <- exprLetRecBindings
  void $ satisfyTok satisfyIn
  expr <- exprExpr
  return $ ExprLetRec bindings expr

exprExpr :: ParsecT [LispToken] () Identity Expr
exprExpr = do
  try exprLet
    <|> try exprTryCatch
    <|> try exprRaise
    <|> try exprLetRec
    <|> try exprLambda
    <|> try exprPrimitivePredicate
    <|> try exprPrimitive
    <|> try exprIf
    <|> try exprApp
    <|> try (satisfyTok satisfyNumeric)
    <|> satisfyTok satisfySymbol

printExpr :: Expr -> Int -> T.Text
printExpr (ExprLitNum x) _indent = T.pack . show $ x
printExpr (ExprVar x) _indent = x
printExpr (ExprLambda ids e) indent =
  let ids' = T.intercalate " " $ L.map unIdent ids
      x = T.pack "(lambda (" <> ids' <> ")"
      y = printExpr e (indent + 2)
   in x <> "\n" <> T.replicate (indent + 2) " " <> y <> ")"
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
printExpr (ExprPrimPred rator expr1 expr2) indent =
  let expr1' = printExpr expr1 indent
      expr2' = printExpr expr2 indent
      op = case rator of
        PrimAnd -> T.pack "&&"
        PrimOr -> T.pack "||"
        --PrimNot -> T.pack "not"
        PrimLt -> T.pack "<"
        PrimLte -> T.pack "<="
        PrimGt -> T.pack ">"
        PrimGte -> T.pack ">="
        PrimEq -> T.pack "=="
   in T.pack "(" <> op <> " " <> expr1' <> " " <> expr2' <> ")"
printExpr (ExprIf test_exp true_exp false_exp) indent =
  let test_exp' = printExpr test_exp (indent + 5)
      true_exp' = printExpr true_exp (indent + 5)
      false_exp' = printExpr false_exp (indent + 5)
      indents = T.replicate (indent + 4) " "
   in T.pack "(if " <> test_exp' <> "\n" <> indents <> true_exp' <> "\n" <> indents <> false_exp' <> ")"
printExpr (ExprLet (Identifier var, var_expr) expr) indent = let
  var_expr' = printExpr var_expr indent
  expr' = printExpr expr (indent + 2)
  in
    T.pack "let " <> var <> " = " <> var_expr' <> " in \n" <> T.replicate (indent + 2) " " <> expr'

--printExpr x _ = error $ show $ "not implemented for " ++ show x

parseExpr :: LispParser Expr
parseExpr = exprExpr

lexAndParse :: String -> ParserResult
lexAndParse s = case lexer s of
  Right toks -> parse parseExpr "" toks
  Left e -> Left e

-- Interpreter

data LispValue
  = LispInt Int
  | LispIdentifier T.Text
  | LispClosure [Identifier] !Expr !AllEnv
  deriving (Show, Eq)

data Env = Env
  { env :: !(M.Map T.Text LispValue),
    parent :: !(Maybe AllEnv)
  }
  deriving (Show, Eq)

data ClosureEnv = ClosureEnv (M.Map T.Text Expr) AllEnv
  deriving (Show, Eq)--AllEnv here is the parent

data AllEnv = SimpleEnv Env | RecEnv ClosureEnv
  deriving (Show, Eq)

data LispError
  = SystemError !T.Text
  | ControlFlow !LispValue
  deriving (Show, Eq)

type InterpreterTIO = ExceptT LispError (StateT AllEnv IO) LispValue

initEnv :: Maybe AllEnv -> AllEnv
initEnv !parent = SimpleEnv $ Env {env = M.empty, parent = parent}

--{-# INLINE initEnv #-}

lookupEnv :: T.Text -> AllEnv -> Maybe LispValue
lookupEnv !k !environ = go' (Just environ)
  where
    go' :: Maybe AllEnv -> Maybe LispValue
    go' (Just (SimpleEnv env)) = go (env)
    go' (Just (RecEnv (ClosureEnv bindings parent))) =
      case M.lookup k bindings of
        Just v -> case v of
          (ExprLambda ids expr) -> Just $! LispClosure ids expr environ
          _ -> error "Non-callable found in closure environment"
        Nothing -> go' (Just parent)
    go' Nothing = Nothing

    go (Env {..}) = case M.lookup k env of
      Just v -> Just $! v
      Nothing -> go' parent

--{-# INLINE lookupEnv #-}

-- updateEnv :: T.Text -> LispValue -> Env -> Maybe Env
-- updateEnv !k !v !s = go (Just $! s)
--   where
--     go (Just s'@Env {..}) = case M.lookup k env of
--       Just _ -> Just s' {env = M.update (\_ -> Just v) k env}
--       Nothing -> do
--         !parent_state <- go parent
--         return $ s' {parent = Just parent_state}
--     go Nothing = Nothing

insertEnv :: T.Text -> LispValue -> AllEnv -> AllEnv
insertEnv !k !v (SimpleEnv s@Env {..}) = SimpleEnv $ s {env = M.insert k v env}
insertEnv _ _ _ = error "insertEnv not implemented for ClosureEnv"

--{-# INLINE insertEnv #-}

multiInsertEnv :: [(T.Text, LispValue)] -> AllEnv -> AllEnv
multiInsertEnv !values (SimpleEnv (s@Env {..})) =
  let new_env = M.fromList values
      !new_env' = M.union new_env env
   in SimpleEnv $ s {env = new_env'}
multiInsertEnv _ _ = error "multiInsertEnv not implemented for ClosureEnv"

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

isTruthy :: LispValue -> Bool
isTruthy (LispInt x) = x > 0
isTruthy _ = False

interpretExpr :: Expr -> InterpreterTIO
interpretExpr (ExprLitNum a) = return $ LispInt a
interpretExpr (ExprVar var) = do
  env <- get
  case lookupEnv var env of
    Just v -> return v
    Nothing -> ExceptT . return . Left $ SystemError $ "undefined var:" <> var
interpretExpr (ExprLambda ids expr) = LispClosure ids expr <$> get
interpretExpr (ExprApp exp exprs) = do
  orig_env <- get
  func <- interpretExpr exp
  -- liftIO $ putStrLn $ "calling "  ++ show func
  -- liftIO $ putStrLn $ show exprs
  case func of
    LispClosure ids expr closure -> do
      args <- mapM interpretExpr exprs
      let !pa = L.zip (L.map unIdent ids) args
      let !s = multiInsertEnv pa (initEnv (Just closure))
      put $! s
      value <- interpretExpr expr
      put $! orig_env
      return $! value
    e -> ExceptT . return . Left $ SystemError $ T.pack "expecting callable: Got" <> T.pack (show e)

interpretExpr (ExprIf test_exp true_exp false_exp) = do
  test <- interpretExpr test_exp
  if isTruthy test then interpretExpr true_exp else interpretExpr false_exp
interpretExpr (ExprPrim prim exprs) = do
  rands <- mapM interpretExpr exprs
  let rands' = traverse convert rands
  case rands' of
    Right (x : xs) -> return $ LispInt $ applyPrim (func prim) x xs
    Right _ -> ExceptT . return . Left $ SystemError $ T.pack "Not enough operands for " <> T.pack (show prim)
    Left e -> ExceptT . return . Left $ SystemError e
  where
    convert (LispInt a) = Right a
    convert x = Left $ T.pack "Invalid rand for primitive type: " <> T.pack (show x)

    applyPrim :: (Int -> Int -> Int) -> Int -> [Int] -> Int
    applyPrim func' i vs = L.foldl' func' i vs

    func PrimAdd = (+)
    func PrimSub = (-)
    func PrimMult = (*)

-- interpretExpr (ExprPrimPred PrimNot [expr]) = do
--   result <- interpretExpr expr
--   if not (isTruthy result) then return (LispInt 1) else return (LispInt 0)
-- interpretExpr (ExprPrimPred PrimNot _) = ExceptT . return . Left $ SystemError "Operand need for `not`"

interpretExpr (ExprPrimPred PrimAnd expr1 expr2) = do
  result <- interpretExpr expr1
  if isTruthy result
    then do
      result2 <- interpretExpr expr2
      if isTruthy result2 then return (LispInt 1) else return (LispInt 0)
    else return (LispInt 0)
interpretExpr (ExprPrimPred PrimOr expr1 expr2) = do
  result <- interpretExpr expr1
  if isTruthy result
    then return (LispInt 1)
    else do
      result2 <- interpretExpr expr2
      if isTruthy result2 then return (LispInt 1) else return (LispInt 0)
interpretExpr (ExprPrimPred PrimLt expr1 expr2) = do
  result1 <- interpretExpr expr1
  result2 <- interpretExpr expr2
  interpretCmp result1 result2 (<)
interpretExpr (ExprPrimPred PrimGt expr1 expr2) = do
  result1 <- interpretExpr expr1
  result2 <- interpretExpr expr2
  interpretCmp result1 result2 (>)
interpretExpr (ExprPrimPred PrimLte expr1 expr2) = do
  result1 <- interpretExpr expr1
  result2 <- interpretExpr expr2
  interpretCmp result1 result2 (<=)
interpretExpr (ExprPrimPred PrimGte expr1 expr2) = do
  result1 <- interpretExpr expr1
  result2 <- interpretExpr expr2
  interpretCmp result1 result2 (>=)
interpretExpr (ExprPrimPred PrimEq expr1 expr2) = do
  result1 <- interpretExpr expr1
  result2 <- interpretExpr expr2
  interpretCmp result1 result2 (==)
interpretExpr (ExprLet (Identifier x, var_expr) expr) = do
  var_expr' <- interpretExpr var_expr
  s <- get
  let env' = initEnv (Just s)
  put $ insertEnv x var_expr' env'
  interpretExpr expr
interpretExpr (ExprLetRec bindings expr) = do
  env <- get
  let closure_map = M.fromList (L.map (\((Identifier x), y)->(x,y)) bindings)
  put $ RecEnv $ ClosureEnv closure_map env
  interpretExpr expr
interpretExpr (ExprTryCatch _ _) = error "TryCatch expression not supported in this version of interpreter"
interpretExpr (ExprRaise _) = error "ExprRaise expression not supported in this version of interpreter"

interpretCmp :: LispValue -> LispValue -> (Int -> Int -> Bool) -> InterpreterTIO
interpretCmp (LispInt x) (LispInt y) op = if x `op` y then return (LispInt 1) else return (LispInt 0)
interpretCmp result1 result2 _ =
  ExceptT . return . Left $
    SystemError $ T.pack "Unsupported comparision for " <> T.pack (show result1) <> " and " <> T.pack (show result2)
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

type Cont a = (a -> InterpreterTIO)

data ContType a = ContNormal (Cont a) | ContExcept (Cont a)

testCont :: Expr -> Expr -> ContType (LispValue) -> LispValue -> InterpreterTIO
testCont true_exp false_exp cont val = do
  if (isTruthy val)
    then interpretCPSExpr true_exp cont
    else interpretCPSExpr false_exp cont

applyCont :: ContType a -> a -> InterpreterTIO
applyCont (ContNormal cont) val = cont val
applyCont (ContExcept cont) val = cont val

interpretCPSExpr :: Expr -> ContType LispValue -> InterpreterTIO
interpretCPSExpr (ExprLitNum a) cont = applyCont cont (LispInt a)
interpretCPSExpr (ExprVar var) cont = do
  env <- get
  case lookupEnv var env of
    Just v -> applyCont cont v
    Nothing -> ExceptT . return . Left $ SystemError $ "undefined var:" <> var
interpretCPSExpr (ExprLambda ids expr) cont = do
  val <- ((LispClosure ids expr) <$> get)
  applyCont cont val
interpretCPSExpr (ExprApp exp exprs) cont = do
  interpretCPSExpr exp (ContNormal (applyFunc exprs cont))
  where
    applyFunc exprs cont func =  do
      case func of
        (LispClosure ids expr closure) -> do
          evalRands exprs (ContNormal (applyArgs ids expr closure cont))
        e -> ExceptT . return . Left $ SystemError $ T.pack "expecting callable: Got" <> T.pack (show e)

    applyArgs ids expr closure cont args = do
      orig_env <- get
      let !pa = L.zip (L.map unIdent ids) args
      let !s = multiInsertEnv pa (initEnv (Just closure))
      put $! s
      interpretCPSExpr expr (ContNormal (\val -> do
                                put $! orig_env
                                applyCont cont val))
      --put $! orig_env
      --applyCont cont value


interpretCPSExpr (ExprIf test_exp true_exp false_exp) cont = do
  interpretCPSExpr test_exp (ContNormal (testCont true_exp false_exp cont))

interpretCPSExpr (ExprPrim prim rands) cont = do
  evalRands rands (ContNormal (primArgsCont prim cont))
  where
    convert (LispInt a) = Right a
    convert x = Left $ T.pack "Invalid rand for primitive type: " <> T.pack (show x)

    applyPrim :: (Int -> Int -> Int) -> Int -> [Int] -> Int
    applyPrim func' i vs = L.foldl' func' i vs

    func PrimAdd = (+)
    func PrimSub = (-)
    func PrimMult = (*)

    primArgsCont :: Primitive -> ContType ( LispValue) -> [LispValue] -> InterpreterTIO
    primArgsCont prim cont args = do
      let args' = traverse convert args  -- this is not CPS style yet
      case args' of
        Right (x : xs) -> applyCont cont $ LispInt $ applyPrim (func prim) x xs
        Right _ -> ExceptT . return . Left $ SystemError $ T.pack "Not enough operands for " <> T.pack (show prim)
        Left e -> ExceptT . return . Left $ SystemError e



-- -- interpretExpr (ExprPrimPred PrimNot [expr]) = do
-- --   result <- interpretExpr expr
-- --   if not (isTruthy result) then return (LispInt 1) else return (LispInt 0)
-- -- interpretExpr (ExprPrimPred PrimNot _) = ExceptT . return . Left $ SystemError "Operand need for `not`"

-- interpretExpr (ExprPrimPred PrimAnd expr1 expr2) = do
--   result <- interpretExpr expr1
--   if isTruthy result
--     then do
--       result2 <- interpretExpr expr2
--       if isTruthy result2 then return (LispInt 1) else return (LispInt 0)
--     else return (LispInt 0)
-- interpretExpr (ExprPrimPred PrimOr expr1 expr2) = do
--   result <- interpretExpr expr1
--   if isTruthy result
--     then return (LispInt 1)
--     else do
--       result2 <- interpretExpr expr2
--       if isTruthy result2 then return (LispInt 1) else return (LispInt 0)

interpretCPSExpr (ExprPrimPred PrimLt expr1 expr2) cont = do
  cmpCont expr1 expr2 cont (<)

interpretCPSExpr (ExprPrimPred PrimGt expr1 expr2) cont = do
  cmpCont expr1 expr2 cont (>)
interpretCPSExpr (ExprPrimPred PrimLte expr1 expr2) cont = do
  cmpCont expr1 expr2 cont (<=)
interpretCPSExpr (ExprPrimPred PrimGte expr1 expr2) cont = do
  cmpCont expr1 expr2 cont (>=)
interpretCPSExpr (ExprPrimPred PrimEq expr1 expr2) cont = do
  cmpCont expr1 expr2 cont (==)

interpretCPSExpr (ExprLet (Identifier x, var_expr) expr) cont = do
  interpretCPSExpr var_expr (ContNormal (add_binding x cont))
  where
    add_binding x cont val = do
      s <- get
      let env' = initEnv (Just s)
      put $ insertEnv x val env'
      interpretCPSExpr expr cont

interpretCPSExpr (ExprLetRec bindings expr) cont = do
  env <- get
  let closure_map = M.fromList (L.map (\((Identifier x), y)->(x,y)) bindings)
  put $ RecEnv $ ClosureEnv closure_map env
  interpretCPSExpr expr cont

interpretCPSExpr (ExprTryCatch try_expr1 handle_expr2) cont = do
  interpretCPSExpr handle_expr2 (ContNormal (handler_cont try_expr1 cont))
  where
    handler_cont try_exp cont handler_val =
      case handler_val of
        (LispClosure _ _ _) -> interpretCPSExpr try_exp (ContExcept (try_cont handler_val cont))
        _ -> ExceptT . return . Left $ SystemError "non-callable provided for try-catch handler"

    try_cont handler_val cont body_val = do
      case handler_val of
        (LispClosure ids expr closure) -> applyArgs ids expr closure cont [body_val]
        e -> ExceptT . return . Left $ SystemError $ T.pack "expecting callable: Got" <> T.pack (show e)

    applyArgs ids expr closure cont args = do
      orig_env <- get
      let !pa = L.zip (L.map unIdent ids) args
      let !s = multiInsertEnv pa (initEnv (Just closure))
      put $! s
      interpretCPSExpr expr (ContNormal (\val -> do
                                put $! orig_env
                                applyCont cont val))
      --put $! orig_env
      --applyCont cont value


interpretCPSExpr (ExprRaise expr1) cont = interpretCPSExpr expr1 cont

cmpCont expr1 expr2 cont cmp_func =
  interpretCPSExpr
    expr1
    (ContNormal ( \val1 ->
        interpretCPSExpr
          expr2
          (ContNormal ( \val2 ->
              interpretCPSCmp val1 val2 cmp_func cont
          ))
    ))

interpretCPSCmp :: LispValue -> LispValue -> (Int -> Int -> Bool) -> ContType ( LispValue) -> InterpreterTIO
interpretCPSCmp (LispInt x) (LispInt y) op cont =
  if x `op` y
    then applyCont cont (LispInt 1)
    else applyCont cont (LispInt 0)
interpretCPSCmp result1 result2 _ _ =
  ExceptT . return . Left $
    SystemError $ T.pack "Unsupported comparision for " <> T.pack (show result1) <> " and " <> T.pack (show result2)

evalRands :: [Expr] -> ContType [LispValue] -> InterpreterTIO
evalRands [] cont = applyCont cont []
evalRands (x : xs) cont = interpretCPSExpr x (ContNormal (evalFirstCont xs cont))

evalFirstCont :: [Expr] -> ContType [LispValue] -> LispValue -> InterpreterTIO
evalFirstCont xs cont val = evalRands xs (ContNormal (evalRestCont val cont))

evalRestCont :: LispValue -> ContType [LispValue] -> [LispValue] -> InterpreterTIO
evalRestCont first_val cont rest_val = applyCont cont (first_val : rest_val)


runCPSInterpreter :: String -> ContType (LispValue) -> IO (Either LispError LispValue)
runCPSInterpreter input cont = do
  let result = lexAndParse input
  case result of
    Right r -> do
      let inter = runExceptT (interpretCPSExpr r cont)
      (final, _) <- runStateT inter (initEnv Nothing)
      case final of
        Right f -> return $ Right f
        Left e -> return $ Left e
    Left e -> error $ show e
