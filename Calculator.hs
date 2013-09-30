import Data.Maybe 

data Op = Add | Sub | Mul | Div | And | Or | Not | Eq | Less | Great
  deriving (Eq, Show)
data Exp = Literal Value
         | Primitive Op [Exp]
         | Variable String
         | If Exp Exp Exp
         | Let [(String, Exp)] Exp
         | Fun [String] Exp
         | Call Exp [Exp]
         | Try Exp Exp

  deriving (Show, Eq)

data Checked a = Good a | Error String
  deriving (Show, Eq)

data Value = Number Int
         | Bool Bool
         | Closure [String] Exp Env
         | ENV Env
  deriving (Eq, Show)
type Env = [(String, Value)]

prim Add  [Number a, Number b] = Number (a+b)
prim Mul  [Number a, Number b] = Number (a*b)
prim Sub  [Number a, Number b] = Number (a-b)
prim Div  [Number a, Number b] = Number (a `div` b)
prim And  [Bool a, Bool b] = Bool (a && b)
prim Or   [Bool a, Bool b] = Bool (a || b)
prim Not  [Bool a] = Bool (not a)
prim Eq   [a, b] = Bool (a == b)
prim Less [Number a, Number b] = Bool (a < b)
prim Great [Number a, Number b] = Bool (a > b)

duplicates [] = False
duplicates (x:xs) = (x `elem` xs) || duplicates xs

checked_prim :: Op -> [Value] -> Checked Value
checked_prim Div (a:b:cs) =
  if b == (Number 0)
  then Error "Divide by zero"
  else Good (prim Div [a,b])
checked_prim op a = Good (prim op a)


eval :: Env -> Exp -> Checked Value
eval e (Literal v)  = Good v

eval e (Primitive op as) = 
  let x = checkList(map (eval e) as) in
    if x 
      then checked_prim op [fromGood(v)|v<-(map (eval e) as)]
      else Error "invalid prim"

eval e (Variable x) = Good (fromJust (lookup x e))

eval e (If a b c)  = 
    case eval e a of
      Error msg -> Error msg
      Good av -> eval e (if fromBool (fromGood(eval e a)) then b else c)

eval e (Let bs b)   = 
  if duplicates (map fst bs) 
  then error "Duplicate let variable"
  else 
    let ok = checkList ([ (eval e d) | (_,d) <- bs ]) in
    if ok
      then eval ([(x, fromGood(eval e d)) | (x,d) <- bs ] ++ e) b
      else Error "Invalid Let bs b"

eval e (Fun vs b) = 
  if duplicates vs
  then error "Duplicate function argument"
  else Good (Closure vs b e)

eval e (Call f as) = 
  let ok = checkList ((map (eval e) as)) in
  if ok 
    then
      case eval e f of
        Good v -> eval e'' b 
                    where (Closure xs b e') = fromGood(eval e f)
                          e'' = zip xs (fromGood'(map (eval e) as)) ++ e'
        Error msg -> Error msg 
    else Error "Invalid Call"

eval e (Try a b) = case eval e a of
    Good av -> Good av
    Error msg -> eval e b


fromBool (Bool b) = b
fromGood (Good b) = b
fromGood'  as        = [fromGood(x) | x <- as]
checkList :: [Checked Value] -> Bool
checkList [] = True
checkList (a:as) = 
  case a of
      Good av -> checkList as
      Error msg -> False

checkListOfEnv e [] = True
checkListOfEnv e (a:as) = 
  case eval e (last a) of
      Good av -> checkListOfEnv e as
      Error msg -> False


toGoodList []     = Good []
toGoodList (v:vs) = case v of
                      Error msg -> Error msg
                      Good gv   -> case toGoodList vs of
                                    Error msg -> Error msg
                                    Good gvs -> Good (gv:gvs)

n = "n"
x = Variable "x"
f = Variable "f"
lit n = Literal (Number n)

t0 = Fun ["x"] (Primitive Mul [x, lit 2])
t1 = Let [("f", t0)] 
         (Primitive Add [Call f [lit 3], Call f [lit 10]])

t3 = Fun ["x", "y"] (Primitive Div [x, lit 0])
t4 = Let [("f", t3)] 
         (Primitive Add [Call f [lit 3], Call f [lit 10]])

main = do
  
  print t3
  print (eval [] t3)
  print (eval [] t4)
  
