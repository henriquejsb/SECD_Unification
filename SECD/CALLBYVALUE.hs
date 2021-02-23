module SECD where
import Data.List(elemIndex)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad

data Instr =     LDC Int   --X
               | LD Int  
               | LDF [Instr]
               | LDRF [Instr]
               | ADD
               | SUB
               | MUL
               | HALT
               | JOIN
               | AP 
               | RTN
               | SEL [Instr] [Instr]
               deriving Show

type Addr = Int

type Closure = (Code, Env)

data Value =     I Int
               | A Addr
               deriving (Eq,Show)

type Code = [Instr]

type Stack = [Value]

type Env = [Value]

type Dump = [(Stack,Env,Code)]

type Store = Map Addr Closure

type SECD = (Stack,Env,Code,Dump,Store)

type Ident = String

data Term = Var Ident               -- variables X
          | Lambda Ident Term       -- abstraction X
          | App Term Term           -- application X
          | Const Int               -- constants X
          | Term :+ Term            -- arithmetic operators X
          | Term :- Term            -- X
          | Term :* Term            -- X
          | IfZero Term Term Term   -- conditional X
          | Let Ident Term Term     -- local definition X
          | Fix Term                -- fixed-point operator
            deriving (Eq, Show)



type Symtable = [Ident]


next::Store -> Addr
next store = 1 + Map.size store



compile :: Term -> Symtable -> Code
compile(App e1 e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [AP]
compile(Const k) sym = [LDC k]
compile(e1 :+ e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [ADD]
compile(e1 :- e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [SUB]
compile(e1 :* e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [MUL]
compile(Var x) sym = case elemIndex x sym of
                    Just v -> [LD v]
                    Nothing -> error ("VARIAVEL " ++ show x ++ " NAO INICIALIZADA ")
-- compile(Lambda x e) sym = 
  --   let k = compile e (x:sym)  ++ [RTN]
    -- in [LDF k]
compile(Lambda x e) sym = [LDF (compile e (x:sym) ++ [RTN])]
compile(IfZero e0 e1 e2) sym = 
     let s0 = compile e0 sym 
         s1 = compile e1 sym ++ [JOIN]
         s2 = compile e2 sym ++ [JOIN]
         in s0 ++ [SEL s1 s2]
compile(Let x e1 e2) sym = compile (App (Lambda x e2) e1) sym
compile(Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e (x:f:sym) ++ [RTN])] 

compiler::Term -> [Instr]
compiler e = compile e [] ++ [HALT]


execute :: SECD -> SECD
execute(s, e, LDC n:c,d,m) = (I n:s, e, c,d,m)
execute(s,e,LD i:c,d,m) = (v:s,e,c,d,m)
                          where v = e!!i
execute(I n2: I n1: s,e,ADD:c,d,m) = (I (n1+n2):s,e,c,d,m)
execute(I n2: I n1: s,e,SUB:c,d,m) = (I (n1-n2):s,e,c,d,m)
execute(I n2: I n1: s,e,MUL:c,d,m) = (I (n1*n2):s,e,c,d,m)
execute(s,e,HALT:c,d,m) = (s,e,c,d,m)
execute(s,e,(LDF c'):c,d,m) = (A addr:s,e,c,d,Map.insert addr (c',e) m) 
                              where addr = next m
execute(s,e,(LDRF c'):c,d,m) = (A addr:s,e,c,d,Map.insert addr (c',A addr:e) m)
                              where addr = next m

execute(v:A a:s,e,AP:c,d,m) =  ([],v:e',c',(s,e,c):d,m)
                              where Just (c',e') = Map.lookup a m

                             
execute(v:s,e,RTN:c,(s',e',c'):d,m) = (v:s',e',c',d,m) 

                            
execute(I n:s,e,(SEL c1 c2):c,d,m)
  | n==0       = (s,e,c1,([],[],c):d,m)
  | otherwise  = (s,e,c2,([],[],c):d,m)
execute(s,e,JOIN:c, (_,_,c'):d,m) = (s,e,c',d,m)



{-




execute(stack, env, LDC k : c, dump) = (Prim k : stack, env, c, dump)
execute(v:(Closure code' env'):stack, env, AP : c, dump)
-- == ([], v: env', code', (stack, env, c): dump)
execute(v:stack', env', RTN: code', (stack, env, c): dump)
-- == (v:stack, env, c, dump)
-}

{-
runner :: SECD -> SECD
runner (s,e,c,d,m) = if null c
               then secd
               else runner (execute secd)
               where secd = (s,e,c,d,m)


getVal::SECD -> Value
getVal (v:s,_,_,_,_) = v

run :: Term -> Value
run e0 = getVal (runner secd)
    where secd = ([],[],compiler e0,[],Map.empty)
-}


runer :: SECD -> [SECD]
runer (s,e,c,d,m) = if null c
               then [secd]
               else secd:runer(execute secd)
               where secd = (s,e,c,d,m)



run :: Term -> [SECD]
run e0 = runer ([],[],compiler e0,[],Map.empty) 


{-
run2 :: Code -> IO ()
run code = do 
          start <- ([],[],code,[],Map.empty) 
            start <- execute start 
            while(not null code)
-}

-- run :: Code -> IO()

-- Code -> Value

