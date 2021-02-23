module TYPEINFERENCE where
import UNIFICATION
import Data.Map(Map)
import qualified Data.Map as Map
import System.Random
import System.IO.Unsafe
import Test.RandomStrings


iso_alpha = onlyAlpha randomChar8
ascii_alphanum = onlyAlphaNum randomASCII

generateVar:: IO String
generateVar = randomString ascii_alphanum 5


data TypeScheme = ForAll String TypeScheme 
    |T Type
    deriving (Eq,Show)

data Expr = ILit Int
            | Var String
            | BLit Bool
            | App Expr Expr
            | Lambda String Expr
            deriving (Eq,Ord,Show)


type TypeEnv = Map String TypeScheme

removeVar::TypeEnv -> Type -> TypeEnv
removeVar env (VType n) = Map.delete n env

--- primeiro subs dps alvo
apply_substitutions::[Substitution] -> Type -> Type
apply_substitutions (s:xs) t = apply_substitutions xs (replace_var t s)
apply_substitutions [] t  = t

-- primeiro arg alvo segundo arg subs
apply_substitutions2::[Substitution] -> [Substitution] -> [Substitution]
apply_substitutions2 s1 (s:s2) = apply_substitutions2 (apply_substitution2 s1 s) s2
apply_substitutions2 s1 [] = s1


check_var_in_sub::Substitution ->String -> Maybe Substitution
check_var_in_sub (x,t) n = if x == n then Nothing else Just (x,t)

replace_var_scheme::Substitution->TypeScheme->TypeScheme
replace_var_scheme s (ForAll var ts) = case check_var_in_sub s var of
                                            Nothing -> (ForAll var ts)
                                            Just s -> (ForAll var (replace_var_scheme s ts))
replace_var_scheme s (T t) = T (replace_var  t s)

apply_substitutions_scheme::[Substitution] -> TypeScheme -> TypeScheme
apply_substitutions_scheme (s:xs) ts = apply_substitutions_scheme xs (replace_var_scheme s ts)
apply_substitutions_scheme [] ts = ts

apply_substitutions_context::TypeEnv ->[Substitution] -> TypeEnv
apply_substitutions_context env s = Map.map (apply_substitutions_scheme s) env




fresh_vars::TypeScheme -> [Substitution] -> Type
fresh_vars (ForAll s ts) subs = let newVar = VType (unsafePerformIO (generateVar) ) in
                                    fresh_vars ts (subs ++ [(s,newVar)])
fresh_vars (T t) subs = apply_substitutions subs t



ti::TypeEnv -> Expr -> ([Substitution],Type)
ti env (ILit n) = ([],I)
ti env (BLit b) = ([],B)
ti env (Var n) = case Map.lookup n env of
                    Nothing -> error "lol"
                    Just sigma -> ([],fresh_vars sigma [])
ti env (Lambda n expr) = let 
                            newVar = VType (unsafePerformIO (generateVar) )
                            env1 = removeVar env newVar
                            env2 = Map.insert n (T newVar) env1 
                            (s1,t1) = ti env2 expr
                        in (s1,(apply_substitutions s1 newVar) :-> t1)
ti env (App e1 e2) = let
                        newVar = VType (unsafePerformIO (generateVar) )
                        (s1,t1) = ti env e1
                        (s2,t2) = ti (apply_substitutions_context env s1) e2
                        in case unify [[(apply_substitutions s2 t1 ), (t2 :-> newVar)]] [] of
                            Nothing -> error "lol"
                            Just v -> (apply_substitutions2 s1 (apply_substitutions2 s2 v), apply_substitutions v  newVar) 

-- (App (App (Lambda "x" (Lambda "y" (Var "x"))) (ILit 1)) (ILit 2))


main2::Expr -> ([Substitution],Type)
main2 e = ti Map.empty e

main::Expr -> Type
main e = let 
		(s,t) = ti Map.empty e
		in apply_substitutions s t


{-

TODO
- GERAR VAR NAMES
- fresh_vars



typeInference :: Map.Map String TypeScheme -> Expr -> Type
typeInference env e = do 
                        (s, t) <- ti (TypeEnv env) e
                        return (apply_substitutions t s)

main::Expr -> IO()
main e = do 
            (res,_) <- typeInference Map.empty e
            putStrLn $ "finish"
            



ti env (Var n) = case Map.lookup n env of
                Nothing -> error ("Unbound variable " ++ n)
                Just sigma -> do t <- 
-}



