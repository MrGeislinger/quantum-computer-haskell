module QuantumComputer.QStates
( Qstate(..)  
, combineStates
, simplify
, removeZeroStates
, grabQbit
, normalize
, findAndCombine
, Complex
, cZero
, cOne
, complexToFloat
, cNegate
, cInv
, cSqAdd
, cNormalize
, ClassicState
) where  

import QuantumComputer.Qbits
import Numbers.Fractions

-- Defines complex (fractions when we can) 
type Complex = Fraction 

-- Define complex zero and one
cZero = SqFrac 0 1 
cOne  = SqFrac 1 1

-- function to convert Complex to a float
complexToFloat :: Complex -> Float
complexToFloat c = sqFracToFloat2 c

-- negate
cNegate :: Complex -> Complex
cNegate (SqFrac a b) = SqFrac a (-b)

-- inverse of a complex number
cInv :: Complex -> Complex
cInv x = fracInv x -- SqFrac inverse


-- square all numbers in list and add together
cSqAdd :: [Complex] -> Complex
cSqAdd x = sqAdd x

-- Builds the quantum state Qstate = [ClassicState]
type ClassicState = (Complex,[Qbit])

-- Qstate is essentially all qbits in a circuit
-- State holds all possible combinations of states
data Qstate = State [ClassicState] deriving (Show)
 



------------------------------
-- Quantum Helper Functions --
------------------------------

-- Combine states together
combineStates :: Qstate -> Qstate -> Qstate
combineStates (State s) (State s') = State $ [ (a*a',q++q') | (a,q) <- s, (a',q') <- s']

-- combines all same ClassicStates together (with factors) while removing resulting zero amplitude ClassicStates
simplify :: Qstate -> Qstate
simplify s@(State [])   = s                               -- empty state
simplify (State [a])    = normalize $ State [a]           -- one state, ensure normalized
simplify (State (a:as)) = normalize $ removeZeroStates $ State $ simp (a:as) -- normalize and remove empty states
                          where simp []     = []
                                simp [x]    = [x] 
                                simp (x:xs) = [fst searched] ++ (simp $ snd searched)
                                        where searched = findAndCombine x xs
                                
                       
-- remove zero amplitude ClassicStates from the Qstate
removeZeroStates (State []) = State []
removeZeroStates (State ( ((SqFrac 0 _),s) : ss ) ) = removeZeroStates $ State ss  -- remove the zero ampitude
removeZeroStates (State (s:ss) ) = (\a (State b) -> State $ [a] ++ b) s (removeZeroStates $ State ss)

-- find where ClassicState matches within [ClassicState] list and combine factors
-- and the non factors in a little package
-- findAndCombine :: (Eq t1, Num t) => (t,t1) -> [(t,t1)] -> [(t,t1)] -> ( (t,t1) , [(t,t1)] )
-- findAndCombine found [] rest = (found , rest ) -- gives a list of all found to be in state
findAndCombine :: ClassicState -> [ClassicState] -> (ClassicState,[ClassicState])
findAndCombine (a,val) list  = ( (foundProb,val) , rest )
                where foundProb    = sqAdd $ map ( \(x,_) -> x) found
                      (found,rest) = find_list [(a,val)]  [] list
                      find_list f@((_,qs):ss) r [q@(a,v)] -- gives lists for those found and the rest in tuple
                                | qs == v = ( q:f , r   )  -- add last state to matches
                                | qs /= v = ( f   , q:r )  -- add last state to not matches
                      find_list founds@( (_,qs):ss ) rests ( s@(_,v):vals )
                                | qs == v = find_list (s:founds) rests vals  -- matches: add to found list
                                | qs /= v = find_list founds (s:rests) vals  -- no match: add to the rest
                      
                      

-- grab a qbit from state (after measurement on that qbit) and return value (0 or 1)
grabQbit :: Qstate -> Int -> Int
grabQbit (State ((_,s):qs)) n = qbitToInt myQbit -- convert measured qbit to 
                       where dummyState = qs  -- grab any state since it was measured (all the same for that qbit)
                             myQbit = s !! n -- get qbit to test

-- normalize by getting normalization factor and dividing terms by such
cNormalize :: [Complex] -> [Complex]
cNormalize a = map ( * (cInv n') ) a  -- divide by normalization factor
               where n' = cSqAdd a   -- sqaure and add numbers


-- normalize by getting normalization factor and dividing terms by such
normalize :: Qstate -> Qstate
normalize (State s) = State $ map (xf) s  -- divide each factor by normalization constant and create new state
                      where n' = cSqAdd $ map ( \(n,q) -> n) s  -- square and add all factors together
                            xf (a,q) = ( a * (cInv n') , q )    -- divide factor by normalization constant













