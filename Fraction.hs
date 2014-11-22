module Numbers.Fractions
( Fraction(..)  
, reduce
, fracInv
, sqAdd
, sqFracToFloat2
) where  

data Fraction = Frac   Int Int
              | SqFrac Int Int -- note that for now, the negative will be carried in the bottom of fraction


instance Show Fraction where       
    show (Frac a 1) = show a
    show (Frac 0 b) = show 0
    show (Frac a' b') = show a ++ "/" ++ show b
                    where (Frac a b) = reduce $ Frac a' b'
-- Squareroot Fractions
    show (SqFrac 0 b) = show 0
    show (SqFrac a 1) 
            | isSquare a = show $ sqrtInt a
            | otherwise  = "√" ++ show a        
    show (SqFrac 1 b) 
            | b > 0 = "1/√"  ++ show b
            | b < 0 = "-" ++ show (SqFrac 1 (-b))     -- test for negative on bottom (negate whole fraction)
    show (SqFrac a' b') 
            | b' < 0 = "-" ++ show (SqFrac a' (-b'))  -- test for negative on bottom (negate whole fraction)
            | (isSquare a) && (isSquare b)  = show $ Frac (sqrtInt a) (sqrtInt b)
            | (isSquare a) = show (sqrtInt a) ++ "/√" ++ show b
            | (isSquare b) = "√" ++ show a ++ "/" ++ (show $ sqrtInt b)
            | otherwise    = "√" ++ show a ++ "/√" ++ show b
            where (SqFrac a b) = reduce $ SqFrac a' b'

instance Eq Fraction where
   (Frac   a b) == (Frac   a' b') = (a*b') == (b*a') -- cross multiply
   (SqFrac a b) == (SqFrac a' b') = (a*b') == (b*a') -- cross multiply

instance Num Fraction where
   (Frac   a b) * (Frac   a' b') = reduce $ Frac   (a*a') (b*b')
   (SqFrac a b) * (SqFrac a' b') = reduce $ SqFrac (a*a') (b*b')
   (Frac a b)   + (Frac   a' b') = reduce $ Frac   (a*b' + a'*b) (b*b')
   (Frac a b)   - (Frac   a' b') = reduce $ Frac   (a*b' - a'*b) (b*b')
   abs    (Frac a b) = Frac (abs a)    (abs b)
   signum (Frac a b) = Frac (signum a) (signum b)
   fromInteger i = reduce $ Frac (fromInteger i) (fromInteger i)


----------------------
-- Other Operations --
----------------------

-- reduce Fractions
reduce :: Fraction -> Fraction
reduce (Frac a b)   
        | a < 0 && b < 0 = reduceMe $ Frac (-a) (-b) -- test if both negative
        |otherwise       = reduceMe $ Frac a b
        where reduceMe (Frac x y) = Frac (div x (gcd x y)) (div y (gcd x y)) -- divide each by their GCD
--reduce (Frac a b)   = Frac (div a n) (div b n)
--                      where n = gcd a b
reduce (SqFrac a b) = SqFrac a' b'
                      where n  = gcd a b
                            a' = div a n
                            b' = div b n

-- make negative 


-- inverts the fraction
fracInv :: Fraction -> Fraction
fracInv (Frac   a b) = reduce $ Frac   b a
fracInv (SqFrac a b)
        | b > 0 = reduce $ SqFrac b a
        | b < 0 = reduce $ SqFrac (-b) (-a)  -- switch negative back to bottom

-- test if number is perfect square                            
isSquare n = sq * sq == n
             where sq = sqrtInt n


-- Squareroot for integers
-- note that it will round down if not perfect square
sqrtInt n = floor $ sqrt $ (fromIntegral n::Double)



-- Square each fraction and then add together
sqAdd :: [Fraction] -> Fraction
sqAdd sqFractions = n' 
                    where a  = map (sqFracToFrac) sqFractions -- square the fractions
                          n  = foldl1 (+) a                   -- add together the squares
                          n' = (fracToSqFrac) n               -- take squareroot of normalization factor 




-- SqFrac to Frac, essentially square the SqFrac
sqFracToFrac :: Fraction -> Fraction
sqFracToFrac (SqFrac a b) = Frac a (abs b) -- remove the neagative after squaring

-- Frac to SqFrac, essentially squareroot the Frac
fracToSqFrac :: Fraction -> Fraction
fracToSqFrac (Frac a b)
        | b < 0     = SqFrac (-a) (-b) -- insert the neagative to top after squareroot
        | otherwise = SqFrac a b       -- both can't be negative; if a<0, keep negative at top


-- square then convert to float
sqFracToFloat2 :: Fraction -> Float
sqFracToFloat2 (SqFrac x y) = (fromIntegral x::Float) / (fromIntegral y::Float)

-- convert squareroot fraction to float
sqFracToFloat :: Fraction -> Float
sqFracToFloat (SqFrac x y) = (sqrt' x) / (sqrt' y)
                             where sqrt' z = sqrt $ (fromIntegral z::Float) -- square root an int to a float












