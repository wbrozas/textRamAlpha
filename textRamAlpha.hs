import Data.List
import Data.Char
import Data.Function
import System.IO
import System.Environment

data Operator = Div | Mult | Plus | Fact deriving (Eq, Enum, Show)
data Exp = Exp { operator :: Operator
				, terms :: [Exp] } 
		 | Term { coefficient :: Double
				, mvars :: String
				, dvars :: String
				} deriving (Eq, Show)

isOperator :: [Char] -> Bool
isOperator (x:_) = x `elem` "+-*/(!"

isVar :: [Char] -> Bool
isVar (x:_) = isAlpha x

precedence :: [Char] -> [Char] -> Bool
precedence (x:_) (y:_) = if (x `elem` "*/" && y `elem` "+-") || (x == '!' && y `elem` "*/+-") || x == '(' then True else False

toPostfix :: [[Char]] -> [[Char]] -> [[Char]]
toPostfix s [] = s
toPostfix [] (x:xs) = if isOperator x then toPostfix [x] xs else x : toPostfix [] xs
toPostfix s@(o:os) (x:xs)
	| isOperator x 	= if o == "(" then toPostfix (x:os) xs else if precedence x o then toPostfix (x:s) xs else o : toPostfix (x:os) xs
	| x == ")" 		= o : toPostfix os xs
	| otherwise 	= x : toPostfix s xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge s@(x:xs) t@(y:ys) = if x < y then x : merge xs t else y : merge s ys

combineVars :: [Char] -> [Char] -> [Char] -> [Char] -> ([Char],[Char])
combineVars xms xds yms yds = (merge (xms \\ yds) (yms \\ xds), merge (xds \\ yms) (yds \\ xms))

postToIr :: [Exp] -> [[Char]] -> Exp
postToIr [y] [] = y
postToIr [] (x:xs) = if isVar x then postToIr ((Term 1 x ""):[]) xs else postToIr ((Term (read x :: Double) "" ""):[]) xs
postToIr s@[y] (x:xs) = if x == "!" then postToIr ((createFactExp y):[]) xs else if isVar x then postToIr ((Term 1 x ""):s) xs else postToIr ((Term (read x :: Double) "" ""):s) xs
postToIr s@(y1:y2:ys) (x:xs) = if x == "!" then postToIr ((createFactExp y1):y2:ys) xs else if isOperator x then postToIr ((createExp x y2 y1):ys) xs else if isVar x then postToIr ((Term 1 x ""):s) xs else postToIr ((Term (read x :: Double) "" ""):s) xs

createFactExp :: Exp -> Exp
createFactExp e = Exp Fact (e:[])

negateExp :: Exp -> Exp
negateExp (Term c ms ds) = Term (-1 * c) ms ds
negateExp (Exp o ts) = Exp o (foldr (\x acc -> ((negateExp x):acc)) [] ts)

createExp :: [Char] -> Exp -> Exp -> Exp
createExp o x y
	| o == "+" = Exp Plus (x:[y])
	| o == "-" = Exp Plus (x:[(negateExp y)])
	| o == "*" = Exp Mult (x:[y])
	| o == "/" = Exp Div (x:[y])

reduce :: Exp -> Exp
reduce e@(Term _ _ _) = e
reduce (Exp o ts)
	| o == Plus = Exp Plus (foldr (\x acc -> (getAllTerms (reduce x)) ++ acc) [] ts)
	| o == Mult = case ts of (x:[y]) -> Exp Plus (distributeMult (reduce x) (reduce y))
	| o == Div = case ts of (x:[y]) -> Exp Plus (distributeDiv (reduce x) (reduce y))
	| o == Fact = case ts of [y] -> Exp Plus ((factTerm (reduce y)):[])

factTerm :: Exp -> Exp
factTerm (Term c "" "") = Term (fact c) "" ""
factTerm e@(Exp Plus ts) = factTerm $ (\(Exp _ [z]) -> z) (addLikeTerms e)
fact 0 = 1
fact n = if n < 0 then -1 * (product [1..(-1 * n)]) else product [1..n]

getAllTerms :: Exp -> [Exp]
getAllTerms e@(Term _ _ _) = [e]
getAllTerms (Exp _ ts) = foldr (\x acc -> (getAllTerms x) ++ acc) [] ts

distributeMult :: Exp -> Exp -> [Exp]
distributeMult (Exp _ ts) (Exp _ t2s) = foldr (\x acc -> (distributeMult' x t2s) ++ acc) [] ts
distributeMult t@(Term _ _ _) (Exp _ ts) = distributeMult' t ts
distributeMult (Exp _ ts) t@(Term _ _ _) = distributeMult' t ts
distributeMult t@(Term _ _ _) t2@(Term _ _ _) = (eval Mult t t2):[]

distributeMult' :: Exp -> [Exp] -> [Exp]
distributeMult' e ys = foldr (\x acc -> (eval Mult e x):acc) [] ys

distributeDiv :: Exp -> Exp -> [Exp]
distributeDiv (Exp _ ts) e@(Term _ _ _) = foldr (\x acc -> (eval Div x e):acc) [] ts
distributeDiv t@(Term _ _ _) t2@(Term _ _ _) = (eval Div t t2):[]
distributeDiv t@(Term _ _ _) e@(Exp Plus _) = case addLikeTerms e of (Exp Plus [z]) -> (eval Div t z):[]
distributeDiv (Exp _ ts) (Exp Plus [z]) = foldr (\x acc -> (eval Div x z):acc) [] ts

addLikeTerms :: Exp -> Exp
addLikeTerms (Exp Plus ts) = Exp Plus (foldr (\x acc -> if isZero x then acc else x:acc) [] $ sortBy (compare `on` (\(Term c _ _ ) -> c)) $ foldr (\x acc -> (addLikeTerms' x):acc) [] $ groupBy varsEqual $ sortBy (compare `on` getVarStr) ts)

addLikeTerms' :: [Exp] -> Exp
addLikeTerms' [t] = t
addLikeTerms' (t:ts) = foldr (\x acc -> eval Plus x acc) t ts

isZero :: Exp -> Bool
isZero (Term c _ _) = c == 0

getVarStr :: Exp -> [Char]
getVarStr (Term _ ms ds) = ms ++ ":" ++ ds

varsEqual :: Exp -> Exp -> Bool
varsEqual (Term _ xms xds) (Term _ yms yds) = if xms == yms && xds == yds then True else False

fromIrToInfix :: Exp -> [Char]
fromIrToInfix e@(Term _ _ _) = fromIrToInfix' e
fromIrToInfix (Exp Plus ts) = fitp ts 

fitp :: [Exp] -> [Char]
fitp [x] = if isNegative x then "( 0 - " ++ fromIrToInfix' (negateTerm x) ++ " )" else fromIrToInfix' x
fitp (x:[y]) = if isNegative y then "( ( 0  - " ++ fromIrToInfix' (negateTerm y) ++ " ) - " ++ fromIrToInfix' (negateTerm x) ++ " )" else if isNegative x then "( " ++ fromIrToInfix' y ++ " - " ++ fromIrToInfix' (negateTerm x) ++ " )" else "( " ++ fromIrToInfix' y ++ " + " ++ fromIrToInfix' x ++ " )"
fitp (x:xs) = if isNegative x then "( " ++ fitp xs ++ " - " ++ fromIrToInfix' (negateTerm x) ++ " )" else "( " ++ fitp xs ++ " + " ++ fromIrToInfix' x ++ " )"

isNegative :: Exp -> Bool
isNegative (Term c _ _) = c < 0

negateTerm :: Exp -> Exp
negateTerm (Term c ms ds) = Term (-1 * c) ms ds

fromIrToInfix' :: Exp -> [Char]
fromIrToInfix' (Term c "" "") = show c
fromIrToInfix' (Term c "" d) = (take (2 * (length d)) $ cycle "( ") ++ (show c) ++ (foldl (\acc x -> " / " ++ [x] ++ " )" ++ acc) [] d)
fromIrToInfix' (Term c m d) = if c == 1 then (take (2 * ((length m) - 1 + (length d))) $ cycle "( ") ++ (head m : (foldl (\acc x -> " * " ++ [x] ++ " )" ++ acc) [] $ tail m)) ++ (foldl (\acc x -> " / " ++ [x] ++ " )" ++ acc) [] d) else (take (2 * (length m) + (length d)) $ cycle "( ") ++ (show c) ++ (foldl (\acc x -> " * " ++ [x] ++ " )" ++ acc) [] m) ++ (foldl (\acc x -> " / " ++ [x] ++ " )" ++ acc) [] d)

eval :: Operator -> Exp -> Exp -> Exp
eval o (Term xc xms xds) (Term yc yms yds)
	| o == Plus = Term (xc + yc) xms xds
	| o == Mult = let a = combineVars xms xds yms yds in Term (xc * yc) (fst a) (snd a)
	| o == Div 	= let a = combineVars xms xds yds yms in Term (xc / yc) (fst a) (snd a)

main :: IO ()
main = do
	args <- getArgs
	putStrLn $ fromIrToInfix $ addLikeTerms $ reduce $ postToIr [] $ toPostfix [] $ words $ head args
