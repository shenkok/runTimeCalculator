newtype Oro = Oro { num :: Rational}
 deriving(Eq)

instance Show Oro where
   show (Oro num) = id $ (show "bernoulli " ++ show num)

a = [[1]]


printEq :: Char -> IO Bool
printEq c = do
            return $ c /= 'y'

b = map printEq "holitas"

solution = do 
         b' <- b
         b''<- b'
         if b'' 
            then do
               print "no"
            else 
               print "yes" 