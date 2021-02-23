module UNIFICATION where


data Type = Type :-> Type | VType String | I | B 
     deriving (Eq,Show)


type Substitution = (String, Type)

-- Verifica se uma variável ocorre num Type
occurs::String -> Type -> Bool
occurs s (t1 :-> t2) = (occurs s t1 ) || (occurs s t2)
occurs s (VType b) = if s == b then True
                    else False
occurs _ _ = False


--Substitui uma variável num Type por outro Type
replace_var::Type->Substitution->Type
replace_var (t1 :-> t2) sub = ((replace_var t1 sub) :-> (replace_var t2 sub))
replace_var (VType a) sub = if a == fst sub then snd sub
                         else VType a
replace_var t sub = t


--Aplica substituição de variáveis numa equação
apply_substitution::[[Type]]->Substitution->[[Type]]
apply_substitution ((t1:t2:ys):xs) sub= ((replace_var t1 sub):(replace_var t2 sub):ys):(apply_substitution xs sub)
apply_substitution [] sub = []

--Aplica algoritmo de unificação a uma equação
unify_aux::[Type] -> Maybe [Substitution]
unify_aux (I:I:xs) = Just []
unify_aux (B:B:xs) = Just []
unify_aux (VType a:VType b:xs) = if a == b then Just []
                                else Just [(a,(VType b))]
unify_aux (t:VType a:xs) = unify_aux (VType a:t:xs)
unify_aux (VType a:t:xs) = if (occurs a t) then Nothing
                            else Just [(a,t)]
unify_aux (_:_:xs) = Nothing

--Aplica substituição de variáveis noutras substituições
apply_substitution2::[Substitution] -> Substitution -> [Substitution]
apply_substitution2 (s:xs) sub = (fst s, (replace_var (snd s) sub)):(apply_substitution2 xs sub)
apply_substitution2 [] sub = [sub]

--Função principal , chama unify_aux , caso devolva uma substituição, aplica substituição nas
--seguintes equações e nas substituições já feitas anteriormente
unify::[[Type]]->[Substitution] -> Maybe [Substitution]
unify (((t11 :-> t12):(t21 :-> t22):ys):xs) subs = unify (([t11] ++ [t21]):([t12] ++ [t22]):xs) subs
unify (eq:xs) subs = do
            case unify_aux eq of 
                Just sub -> if length sub > 0 then 
                    unify (apply_substitution xs (head sub)) (apply_substitution2 subs (head sub))
                    else unify xs subs
                Nothing -> Nothing
unify [] subs = Just subs




{-
 unify [[VType "b" :-> (I :-> B) :-> VType "c",VType "c" :-> VType "a" :-> I]] []

unify [[VType "b" :-> VType "a", I :-> B]] []
unify [[VType "b",VType "a"],[VType "a",I],[VType "c",VType "b" :-> VType "a" ]] []
unify [[VType "b",VType "a"],[VType "a",I],[VType "c",VType "b" :-> VType "a" ]] []
[[VType "b",I:-> VType "a"],[VType "a",I:->B]]
[[VType "a",I:->B],[VType "b",I:-> VType "a"]]
unify::Type->Type->[[Type]]
unify a b = convertToEquations (convertToList a)  (convertToList b)


TypeScheme = Type | ForAll TVar Type

-}
