import           Data.Int    (Int32)
import           Data.String (IsString (..))
import           Data.Set (Set)
import qualified Data.Set as Set

newtype Name = Name String deriving (Eq, Ord)

instance Show Name where show (Name s) = s

instance IsString Name where fromString = Name


data AExp = Lit Int32
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp 
          | AExp :*: AExp deriving (Show, Eq)

sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _  _ (Lit n) = (Lit n)
sustAExp x  aritFor (Var y) = if (x == y) then aritFor else (Var y)
sustAExp x aritFor (e_1 :+: e_2) = (sustAExp x aritFor e_1) :+: (sustAExp x aritFor e_2)
sustAExp x aritFor (e_1 :-: e_2) = (sustAExp x aritFor e_1) :-: (sustAExp x aritFor e_2)
sustAExp x aritFor (k :*: e_2) = k :*: (sustAExp x aritFor e_2)


data BExp = True'
          | False'
          | AExp :<=: AExp
          | AExp :==: AExp
          | BExp :|: BExp
          | BExp :&: BExp
          | Not BExp deriving (Show, Eq)

sustBExp :: Name -> AExp -> BExp -> BExp
sustBExp _  _ True' = True'
sustBExp _  _ False' = False'
sustBExp x aritFor (e_1 :<=: e_2) = (sustAExp x aritFor e_1) :<=: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :==: e_2) = (sustAExp x aritFor e_1) :==: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :|: e_2) = (sustBExp x aritFor e_1) :|: (sustBExp x aritFor e_2)
sustBExp x aritFor (e_1 :&: e_2) = (sustBExp x aritFor e_1) :&: (sustBExp x aritFor e_2)
sustBExp x aritFor (Not e) = (Not (sustBExp x aritFor e))

data RunTime = RunTimeArit AExp
          | BExp :<>: RunTime
          | RunTime :++: RunTime
          | RunTime :--: RunTime 
          | RunTime :**: RunTime  deriving (Show, Eq)


sustRuntime :: Name -> AExp -> RunTime -> RunTime
sustRuntime x aritFor (RunTimeArit aritIn) = (RunTimeArit (sustAExp x aritFor aritIn))
sustRuntime x aritFor (e_b :<>: e_r) = (sustBExp x aritFor e_b) :<>: (sustRuntime x aritFor e_r)
sustRuntime x aritFor (e_1 :++: e_2) = (sustRuntime x aritFor e_1) :++: (sustRuntime x aritFor e_2)
sustRuntime x aritFor (e_1 :--: e_2) = (sustRuntime x aritFor e_1) :--: (sustRuntime x aritFor e_2)
sustRuntime x aritFor (e_1 :**: e_2) = (sustRuntime x aritFor e_1) :**: (sustRuntime x aritFor e_2)

data Program = Skip
            | Empty
            | Set Name AExp
            | Seq Program Program
            | If BExp Program Program
            | While BExp Program RunTime deriving(Show, Eq)

data Restriction = RunTime :!==: RunTime
               | RunTime :!<=: RunTime deriving(Show, Eq)



vcGenerator ::  Program -> RunTime -> (RunTime, [Restriction])
vcGenerator Skip runt = ((RunTimeArit (Lit 1)) :++: runt, [])
vcGenerator Empty runt = (runt, [])
vcGenerator (Set x arit ) runt = ((RunTimeArit (Lit 1)) :++: (sustRuntime x arit runt), [])
vcGenerator (If e_b e_t e_f) runt = ( (RunTimeArit (Lit 1)) :++:((e_b :<>:fst vc_t):++:((Not e_b):<>: fst vc_f)),  (snd vc_t) ++ (snd vc_f)) where 
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (Seq p_1 p_2) runt = (fst vc_1, (snd vc_1) ++ (snd vc_2))   where
    vc_2 = vcGenerator p_2 runt
    vc_1 = vcGenerator p_1 (fst vc_2)
vcGenerator (While e_b p inv) runt = (inv, [l_inv :!<=: inv] ++ (snd vc_p)) where
    vc_p = vcGenerator p inv
    l_inv = (RunTimeArit (Lit 1) :++: ( ((Not e_b) :<>: runt):++: (e_b :<>: (fst vc_p))))


-- Para probar --
b = sustAExp (Name "Hola") (Lit 6) (Lit 5) 
c = sustAExp (Name "Hola") (Lit 6 )  (Var (Name "Hola"))
d = b :+: c

