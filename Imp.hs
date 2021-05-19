import           Data.String (IsString (..))
import           Data.Set (Set)
import qualified Data.Set as Set

type Name = String

data AExp = Lit Integer
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp 
          | Integer :*: AExp deriving (Eq) -- Poner constante

instance Show AExp where 
  show (Lit n) = show n
  show (Var x) = show x
  show (e_1 :+: e_2) = show e_1 ++" + "++ show e_2
  show (e_1 :-: e_2) = show e_1 ++" - "++ show e_2
  show (e_1 :*: e_2) = show e_1 ++" * "++ show e_2       

sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _  _ (Lit n) = (Lit n)
sustAExp x  aritFor (Var y) = if (x == y) then aritFor else (Var y)
sustAExp x aritFor (e_1 :+: e_2) = (sustAExp x aritFor e_1) :+: (sustAExp x aritFor e_2)
sustAExp x aritFor (e_1 :-: e_2) = (sustAExp x aritFor e_1) :-: (sustAExp x aritFor e_2)
sustAExp x aritFor (k :*: e) = k :*: (sustAExp x aritFor e)


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
             | Integer :**: RunTime  deriving (Eq) -- Cambiar a escalar

instance Show RunTime where 
  show (RunTimeArit arit) = show arit
  show (e_b :<>: (RunTimeArit (Lit 1))) = "["++ (show e_b) ++ "]"
  show (e_b :<>: (RunTimeArit (Lit n))) = "["++ (show e_b) ++ "]*" ++ (show n)
  show (e_b :<>: runt) = "["++ (show e_b) ++ "]*" ++ "(" ++ (show runt) ++ ")"
  show (e_1 :++: e_2) = show e_1 ++" + "++ show e_2   
  show (e_1 :**: e_2) = show e_1 ++" * "++ "(" ++ show e_2 ++")"       

sustRuntime :: Name -> AExp -> RunTime -> RunTime
sustRuntime x aritFor (RunTimeArit aritIn) = (RunTimeArit (sustAExp x aritFor aritIn))
sustRuntime x aritFor (e_b :<>: e_r) = (sustBExp x aritFor e_b) :<>: (sustRuntime x aritFor e_r)
sustRuntime x aritFor (e_1 :++: e_2) = (sustRuntime x aritFor e_1) :++: (sustRuntime x aritFor e_2)
sustRuntime x aritFor (k :**: e) = k :**: (sustRuntime x aritFor e)

data Program = Skip
            | Empty
            | Set Name AExp
            | Seq Program Program
            | If BExp Program Program
            | While BExp Program RunTime deriving(Show, Eq)

data Restriction = RunTime :!==: RunTime
               | RunTime :!<=: RunTime deriving(Show, Eq)

---------------------------(vc gen)---------------------------------------------------------

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

---------------------------(vc gen)---------------------------------------------------------

---------------------------(Simplificar)---------------------------------------------------------

simplifyRuntime :: RunTime -> RunTime
simplifyRuntime (True' :<>: runt) = runt
simplifyRuntime (False' :<>: _ )  = (RunTimeArit (Lit 0))
simplifyRuntime ( _ :<>: (RunTimeArit (Lit 0))) = RunTimeArit (Lit 0)
simplifyRuntime ((RunTimeArit (Lit 0)) :++: runt) = runt
simplifyRuntime (runt :++: (RunTimeArit (Lit 0))) = runt 
simplifyRuntime ((RunTimeArit (Lit n)) :++: (RunTimeArit (Lit m))) = (RunTimeArit (Lit (n + m)))
simplifyRuntime ( m :**: (RunTimeArit (Lit n))) = (RunTimeArit (Lit (m * n)))
simplifyRuntime ( 1 :**: runt) = runt
simplifyRuntime ( 0 :**: _ ) = RunTimeArit (Lit 0)
simplifyRuntime otherwise = otherwise

deepsimplifyRuntime :: RunTime -> RunTime
deepsimplifyRuntime (RunTimeArit arit) = (RunTimeArit arit)
deepsimplifyRuntime (bexp :<>: runt) = simplifyRuntime (bexp :<>: (deepsimplifyRuntime runt))
deepsimplifyRuntime (e_1 :++: e_2) = simplifyRuntime ((deepsimplifyRuntime e_1) :++: (deepsimplifyRuntime e_2))
deepsimplifyRuntime (k :**: runt) = simplifyRuntime (k :**: (deepsimplifyRuntime runt))

---------------------------(Simplificar)---------------------------------------------------------

---------------------------(Preparación para z3)-------------------------------------------------

type Context = [BExp]
findConditionRuntime :: RunTime -> Context
findConditionRuntime (RunTimeArit _) = []
findConditionRuntime (bexp :<>: runt) = [bexp] ++ (findConditionRuntime runt)
findConditionRuntime (e_1 :++: e_2) = (findConditionRuntime e_1) ++ (findConditionRuntime e_2)
findConditionRuntime (_ :**: runt) = findConditionRuntime runt

findConditionRestriction :: Restriction -> Context
findConditionRestriction (e_1 :!==: e_2) = (findConditionRuntime e_1) ++ (findConditionRuntime e_2)
findConditionRestriction (e_1 :!<=: e_2) = (findConditionRuntime e_1) ++ (findConditionRuntime e_2)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) r ++ map (True:) r where
  r = bools (n-1)


allConditions :: Restriction -> [[(BExp, Bool)]]
allConditions rest = map (zip conds) vals where
      conds = findConditionRestriction rest
      vals = bools (length conds)

simplifyConditionsContext :: (BExp, Bool) -> BExp
simplifyConditionsContext (bexp, True) = bexp
simplifyConditionsContext (bexp, False) = (Not bexp)

simplifyAllConditions :: [[(BExp, Bool)]] -> [Context]
simplifyAllConditions allvals = map (map simplifyConditionsContext) allvals where



restrictionsToZ3 :: Restriction -> ([Context], Restriction)
restrictionsToZ3 rest =  (allcontext, rest) where
        allcontext = (simplifyAllConditions.allConditions) rest


---------------------------(Preparación para z3)---------------------------------------------------------

---------------------------(Programas de ejemplo)---------------------------------------------------------

-- programa de ejemplo 

p0 = (While False' Empty (RunTimeArit (Lit 5)))
vc_0 = vcGenerator p0 (RunTimeArit (Lit 0)) 
r0 = fst vc_0
s0 = deepsimplifyRuntime (fst vc_0)

-- programa 1 de ejemplo

l0 = Set  "x" ((Var "x"):-:(Lit 1))
l3 = Set "y" (2:*:(Var "x"))
l6 = Set "w" (Lit 3)
l7 = Set "x" ((Var  "w") :+: (Var "x"))
l9 = Set "y" (Lit 5)
l5_9 = If ((Lit 8) :<=: (Var "w")) (Seq l6 l7) l9
l1_9 = If ((Var "y") :<=: (Var "x")) (Seq Skip l3) l5_9
l0_9 = Seq l0 l1_9

vc_1 = vcGenerator l0_9 (RunTimeArit (Lit 0))
s1 = deepsimplifyRuntime (fst vc_1)


---------------------------(Restricciones de ejemplo)---------------------------------------------------------

c1 = (2 :*:(Var "x")):<=:(Var "y")
c2 = (Var "x"):<=:(Lit 0)
c3 = (Var "x"):<=:(Lit 0)
c4 = (Var "ww"):<=:(Lit 0)

arit1 = (Var "x") :+: (Var "y")
arit2 = (Var "y") :+: (Lit 1)

runt1 = c1 :<>: (RunTimeArit arit1)
runt2 = c2 :<>: (RunTimeArit arit2)

restriction = runt1 :!<=: runt2

conds = findConditionRestriction restriction

vals = bools (length conds)

allv = map (zip conds) vals
allc = allConditions restriction

z3Input = restrictionsToZ3 restriction
