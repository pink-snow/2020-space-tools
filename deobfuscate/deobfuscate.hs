import Data.List (elemIndex, groupBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Debug.Trace
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Types

data Operation =
  Oadd|Ob|Oc|Ocar|Ocdr|Ocons|Odiv|Oeq|Oi|Oisnil|Olt|Omul|Oneg|Onil|Os|Ot|Of
  deriving (Eq, Enum)

operationNames =
  ["add","b","c","car","cdr","cons","div","eq","i","isnil","lt","mul","neg","nil","s","t","f"]

data Expr
  = Ap Expr Expr
  | Number Integer
  | Op Operation
  | Var String
  | DefValue String
  | Cons Expr Expr
  | Nil
  | Lambda String Expr
  deriving Eq

data ExprType = ETAny | ETList | ETNum | ETBottom

--------------------------------------------------------------------------------
-- simplify

simplify = fixEq simplify'

simplify' (Ap x y) = fixJust simplifyStep (Ap (simplify' x) (simplify' y))
simplify' (Cons x y) = fixJust simplifyStep (Cons (simplify' x) (simplify' y))
simplify' x = fixJust simplifyStep x

simplifyStep :: Expr -> Maybe Expr
--{{{
simplifyStep (Ap (Ap (Op Oadd) (Number 0)) y) = Just $ y
simplifyStep (Ap (Ap (Op Oadd) x) (Number 0)) = Just $ x
simplifyStep (Ap (Ap (Op Oadd) (Number x)) (Number y)) = Just $ Number (x+y)

simplifyStep (Ap (Ap (Op Omul) (Number 0)) y) = Just $ Number 0
simplifyStep (Ap (Ap (Op Omul) x) (Number 0)) = Just $ Number 0
simplifyStep (Ap (Ap (Op Omul) (Number 1)) y) = Just $ y
simplifyStep (Ap (Ap (Op Omul) x) (Number 1)) = Just $ x
simplifyStep (Ap (Ap (Op Omul) (Number x)) (Number y)) = Just $ Number (x*y)

simplifyStep (Ap (Ap (Op Odiv) x) (Number 1)) = Just $ x
simplifyStep (Ap (Ap (Op Odiv) (Number x)) (Number y)) = Just $ Number (x `quot` y)

simplifyStep (Ap (Ap (Op Oeq) x) y) | x == y = Just (Op Ot)

simplifyStep (Ap (Ap (Op Olt) (Number x)) (Number y))
  | x < y     = Just $ Op Ot
  | otherwise = Just $ Op Of

simplifyStep (Ap (Op Oneg) (Number x)) = Just $ Number (-x)

simplifyStep (Ap (Ap (Ap (Op Os) op1) op2) x) = Just $ Ap (Ap op1 x) (Ap op2 x)

simplifyStep (Ap (Ap (Ap (Op Oc) op1) x) y) = Just $ Ap (Ap op1 y) x

simplifyStep (Ap (Ap (Ap (Op Ob) x) y) z) = Just $ Ap x (Ap y z)

simplifyStep (Ap (Op Oi) x) = Just $ x

simplifyStep (Ap (Ap (Op Ocons) x0) x1) = Just $ Cons x0 x1

simplifyStep (Ap (Op Ocar) (Cons x _)) = Just $ x

simplifyStep (Ap (Op Ocdr) (Cons _ y)) = Just $ y

simplifyStep (Op Onil) = Just $ Nil

simplifyStep (Ap (Op Oisnil) Nil) = Just $ Op Ot
simplifyStep (Ap (Op Oisnil) (Cons _ _)) = Just $ Op Of

simplifyStep (Ap (Ap (Op Of) _) arg2) = Just $ arg2

simplifyStep (Ap (Ap (Op Ot) arg1) _) = Just $ arg1

simplifyStep (Ap (Cons x y) z) = Just $ Ap (Ap z x) y
simplifyStep (Ap Nil z) = Just $ Op Ot

simplifyStep x = Nothing
--}}}



--------------------------------------------------------------------------------
-- Lambdify

trace' x = trace ("{" ++ show x ++ "}") x

lambdify = lambdify' 0

lambdify' used e | argc > 0 =
  addLambdas $ lambdify' (used + argc) $ simplify $ addAps $ e

  where
  argc = canApply e

  canApply (Ap op _) = max (canApply op - 1) 0
  canApply (Op Ob) = 3
  canApply (Op Oc) = 3
  canApply (Op Os) = 3
  canApply (Op Ot) = 2
  canApply (Op Of) = 2
  canApply _ = 0

  vars = map show [used..used+argc-1]

  addLambdas e = foldr (\var e -> Lambda var e) e vars
  addAps e = foldr (\var e -> Ap e (Var var)) e vars

lambdify' used (Ap a b) = Ap (lambdify' used a) (lambdify' used b)
lambdify' used (Cons a b) = Cons (lambdify' used a) (lambdify' used b)
lambdify' _ a = a

--------------------------------------------------------------------------------
--

fixJust :: (a -> Maybe a) -> a -> a
fixJust f a = fix' a (f a)
  where
  fix' a Nothing = a
  fix' _ (Just a) = fix' a (f a)

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f a = fix a (f a)
  where
  fix a b | a == b = a
          | otherwise = fix b (f b)

--------------------------------------------------------------------------------
-- Show

instance Show Expr where
  show = showExpr Map.empty 0

-- 0,1    - Ap
-- 2,3    - + -
-- 4,5    - * /
-- 6      - neg

setop curr target str
  | curr > target = "(" ++ str ++ ")"
  | otherwise = str

showExpr :: DeobfuscationInfo -> Int -> Expr -> String
showExpr m p (Ap (Ap (Op Oadd) a) (Ap (Op Oneg) b)) = setop p 2 $
  showExpr m 2 a ++ " - " ++ showExpr m 2 b
showExpr m p (Ap (Ap (Op Oadd) a) b) = setop p 2 $
  showExpr m 2 a ++ " + " ++ showExpr m 2 b

showExpr m p (Ap (Ap (Op Omul) a) b) = setop p 4 $
  showExpr m 4 a ++ " * " ++ showExpr m 5 b
showExpr m p (Ap (Ap (Op Odiv) a) b) = setop p 4 $
  showExpr m 4 a ++ " / " ++ showExpr m 5 b

showExpr m p (Ap (Op Oneg) a) = setop p 6 $
  '-' : showExpr m 6 a

showExpr m p (Ap a b) = setop p 10 $
  showExpr m 10 a ++ " " ++ showExpr m 11 b

showExpr m p (Number n) = show n
showExpr m p (Op op) = showOperation op
showExpr m p (Var var) = '$' : var
showExpr m p (DefValue def) = ':' : def
showExpr m p e@(Cons _ _) | Just list <- showExprList m e = "[" ++ list ++ "]"
showExpr m p (Cons a b) = "(" ++ showExpr m 0 a ++ ", " ++ showExpr m 0 b ++ ")"
showExpr m p Nil = "nil"

showExpr m p l@(Lambda _ _) = setop p 0 $ showExprLambda m [] l


showExprList m (Cons a Nil) = Just $ showExpr m 0 a
showExprList m (Cons a b) | Just b <- showExprList m b = Just $ showExpr m 0 a ++ ", " ++ b
showExprList m Nil = Just ""
showExprList m _ = Nothing

showExprLambda m vars (Lambda var e) = showExprLambda m (var:vars) e
showExprLambda m vars e = "λ" ++ concatMap (\x -> "$" ++ x ++ " ") (reverse vars) ++ "-> " ++ showExpr m 0 e

{-
showExprAp expr nvar
  | ap1 == ap1' && ap2 == ap2'
    = vars ++ " -> " ++ showExpr expr ++ "\n\n"
  | otherwise = showExprAp ap1' (nvar+1)
  where

  ap1 = Ap expr (Var (show nvar))
  ap1' = simplify ap1

  ap2 = Ap (Ap expr (Var (show nvar))) (Var (show nvar))
  ap2' = simplify ap2

  vars = concatMap (\n -> " $" ++ show n) [0..nvar-1]
-}

showOperation :: Operation -> String
showOperation op = operationNames !! (fromEnum op)

readOperation :: String -> Maybe Operation
readOperation name = fmap toEnum $ elemIndex name operationNames

--------------------------------------------------------------------------------
-- Parse

parse :: String -> Expr
parse s = case parse' (tokenize s) of
  Just (r, []) -> r
  _ -> error "can't parse"

parse' :: [String] -> Maybe (Expr, [String])
parse' ("ap":rest) = do
  (a, rest) <- parse' rest
  (b, rest) <- parse' rest
  return $ (Ap a b, rest)
parse' ("(":rest) = do
  (a, rest) <- parse' rest
  parseParens a rest
  where
  parseParens acc (")":rest) = Just (acc, rest)
  parseParens acc rest = do
    (a, rest) <- parse' rest
    parseParens (Ap acc a) rest
parse' (('$':var):rest) = Just (Var var, rest)
parse' ((':':n):rest) = Just (DefValue n, rest)
parse' (number:rest) | Just num <- readMaybe number = Just (Number num, rest)
parse' (op:rest) | Just op <- readOperation op = Just (Op op, rest)
parse' x = Nothing


tokenize x = words (spaceParens x)
  where
  spaceParens ('(':rest) = ' ' : '(' : ' ' : spaceParens rest
  spaceParens (')':rest) = ' ' : ')' : ' ' : spaceParens rest
  spaceParens (x:rest) = x : spaceParens rest
  spaceParens [] = []

parseGalaxy = map (parseLine . words) . lines
  where
  parseLine ((':':name): "=": rest) = (name, unwords rest)
  parseLine (name: "=": rest) = (name, unwords rest)

showGalaxyFunction di (name, defn)
  = concatMap showCase cases ++ "\n"
  where
  (name', cases') = fromMaybe (Nothing, []) $ Map.lookup name di
  name'' = fromMaybe name name'
  cases
    | cases' == [] = [""]
    | otherwise = cases'

  showCase :: String -> String
  showCase case' = id
    . (\txt -> ":" ++ name'' ++ " " ++ case' ++ " = " ++ txt ++ "\n")
    . showExpr di 0 . lambdify . simplify . parse 
    $ "(" ++ defn ++ " " ++ case' ++ ")"

main = do
  [galaxyFname, dataFname] <- getArgs
  galaxy <- readFile galaxyFname
  di <- parseDeobfuscationInfo <$> readFile dataFname
  putStrLn $ concatMap (showGalaxyFunction di) $ parseGalaxy galaxy

--------------------------------------------------------------------------------

type DeobfuscationInfo = Map String (Maybe String, [String])

parseDeobfuscationInfo = id
  . Map.fromList
  . map handleGroup
  .  groupBy (\_ x -> head x == ' ')
  . filter (not . isEmpty)
  . map removeComment
  . lines
  where
  removeComment ('#':_) = []
  removeComment (x:xs) = x : removeComment xs
  removeComment [] = []
  isEmpty = all (' ' ==)

  handleGroup ((':':name): cases) = (name, foldr handleGroupCase (Nothing, []) cases)
  handleGroupCase (' ':' ':':':' ':rest) (a, b) = (a, rest:b)
  handleGroupCase (' ':' ':'n':' ':rest) (a, b) = (Just rest, b)
  handleGroupCase x _ = error $ "unknown case " ++ x
