module Math where

-- | An algebraic expression, e.g. 2j^k + -4l^2 + -m + log_2(n), which ends up looking like this:
-- Add [
--   Mult [Coeff 2, Exp (Var "j") (Var "k")],
--   Mult [Coeff -4, Exp (Var "l") (Coeff 2)],
--   Mult [Coeff -1, Var "m"],
--   Log 2 (Var "n")
--   
--https://gist.github.com/altaic/164653cdf45366eb294b
--]
data AlgExpr =
  Coeff Rational |      -- ^ Coefficient, e.g. -2
  Var String |          -- ^ Variable, e.g. x
  Mult [AlgExpr] |      -- ^ List to be multiplied, e.g. a*b*c*...
  Add [AlgExpr] |       -- ^ List to be added, e.g. a+b+c+...
  Exp AlgExpr AlgExpr | -- ^ Exponent, e.g. a^b
  Log Rational AlgExpr  -- ^ Logarithm, e.g. log_2(b)
  deriving (Eq)

instance Show AlgExpr where
  show (Coeff c    ) = show (fromRational c)
  show (Var v      ) = v
  show (Mult []    ) = show ""
  show (Mult (e:[])) = show e
  show (Mult (e:es)) = (show e) ++ "⋅" ++ (show (Mult es))
  show (Add []     ) = show ""
  show (Add (e:[]) ) = show e
  show (Add (e:es) ) = (show e) ++ " + " ++ (show (Add es))
  show (Exp b e    ) = (show b) ++ "^(" ++ show e ++ ")"
  show (Log b e    ) = "log_" ++ (show (fromRational b)) ++ "(" ++ (show e) ++ ")"

-- | Squashes algebraic expressions in an ugly way. WIP!
simplify :: AlgExpr -> AlgExpr
simplify (Mult es) = Mult (map (\e -> simplify e) (simplifyMultCoeff (simplifyMultFlatten es []) 1))
simplify (Exp b e) = simplifyExpLog (Exp (simplify b) (simplify e))
simplify (Log b e) = simplifyLogExp (Log b (simplify e))
simplify e = e

-- | This applies the simplification log_a(b^c)) => c*log_a(b).
simplifyLogExp :: AlgExpr -> AlgExpr
simplifyLogExp (Log b (Exp bb ee)) = Mult [simplifyLogExp ee, Log b (simplifyLogExp bb)]
simplifyLogExp (Log b (Mult es)) = Mult (map (\e -> simplifyLogExp (Log b e)) es)
simplifyLogExp e = e

-- | This applies the simplification a^(log_a(b)) => b.
simplifyExpLog :: AlgExpr -> AlgExpr
simplifyExpLog (Exp b (Log bb ee)) = if (b == (Coeff bb)) then (simplifyExpLog ee) else (Exp (simplifyExpLog b) (Log bb (simplifyExpLog ee)))
simplifyExpLog (Exp b (Mult es)) = Mult (map (\e -> simplifyExpLog (Exp (simplifyExpLog b) e)) es)
simplifyExpLog e = e

-- | This applies the simplification a*b*(c*(d*e)*f) => a*b*c*d*e*f. Note that it assumes the list
-- of AlgExpr are to be multiplied together.
simplifyMultFlatten :: [AlgExpr] -> [AlgExpr] -> [AlgExpr]
simplifyMultFlatten ((Mult es):ess) acc = simplifyMultFlatten ess (simplifyMultFlatten es acc)
simplifyMultFlatten (e:es) acc = e : (simplifyMultFlatten es acc)
simplifyMultFlatten [] acc = acc

-- | This applies the simplification 2*x*3*y*4 => 24*x*y. Note that it assumes the list
-- of AlgExpr are to be multiplied together.
simplifyMultCoeff :: [AlgExpr] -> Rational -> [AlgExpr]
simplifyMultCoeff ((Coeff n):es) acc = simplifyMultCoeff es (n * acc)
simplifyMultCoeff (e:es) acc = e : (simplifyMultCoeff es acc)
simplifyMultCoeff [] acc = [Coeff acc]