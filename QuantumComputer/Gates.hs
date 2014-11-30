module QuantumComputer.Gates
( Qgate(..) 
, Qstate(..) 
, Qbit(..) 
, Complex
, ClassicState
, applyGate  
, hSplit
, applyGateN 
, gateAfterMeas 
, xgateM
, zgateM
, hgateM
, xgate
, zgate
, hgate
, cnot 
, meas
) where  


import QuantumComputer.QStates
import QuantumComputer.Qbits
import Numbers.Fractions

-- All the Qgates to be used in a circuit
data Qgate =  I     Int         -- returns the same qbit
            | Xgate Int         -- flips UpDown a b -> UpDown b a
            | Zgate Int         -- makes UpDown a b -> UpDown a (-b)
            | Hgate Int         -- makes superposition state
            | CNOT  Int   Int   -- CNOT  target control (if control is 1 then flip target)
            | CCNOT [Int] [Int] -- CCNOT controls targets (if all controls are 1 then flip targets)
            | Meas  Int   Float -- Forces a qbit to be in only one state (random)
             deriving (Show)


applyGate :: Qgate -> Qstate -> Qstate
applyGate _ (State [])      = State []

-- Identity gate
applyGate (I _) state = state

-- Xgate flips |0> <-> |1>
applyGate (Xgate n) (State x) = simplify $ State $ map (\(a,s) -> (a, xSwitch s)) x  
                                where xSwitch (sl) = replaceN n (flipQbit $ sl !! n) sl 
                                       
-- Zgate changes factor of |1> a to (-a)
applyGate (Zgate n) (State x) = simplify $ State $ map (zSwitch) x
                                where zSwitch (a,c) = if (c!!n == Up) then (cNegate a,c) else (a,c) -- make negative Complex

-- Hgate splits part (need to simplify after)
applyGate (Hgate n) (State x)  = simplify $ State $ foldl (++) [] $ map (hSplit n) x


-- CNOT will flip target if control is |1>
applyGate (CNOT t1 c1) (State x) = State $  map (\q -> cSwitch q) x 
                                       where cSwitch s@(a,qs)
                                                |(qs!!c1) == Down = (a, qs)
                                                |(qs!!c1) == Up   = (\(State [y]) -> y) $ applyGate (Xgate t1) $ State [s]


-- Meas will force a qbit to be either |1> or |0>
applyGate (Meas n r) (State x) = simplify $ State $ removeStates x
                      where removeStates []           = []
                            removeStates ((a,q):ss)
                              | q!!n == measuredState = (a,q) : removeStates ss
                              | q!!n /= measuredState = removeStates ss
                            measuredState = choose r probs -- randomly chooses either Up/Down (starts with 0)
                            probs = ( cSqAdd probsUps , cSqAdd probsDowns) -- get a sum of probabilities for Up and Down 
                            (probsUps,probsDowns) = grab ([],[]) x  
                            grab (as,bs) []  = (as,bs) -- return the lists found for Up and Down (respectively)
                            grab (as,bs) ((c,q):ss)    -- add factor to list in right spot
                                | q!!n == Down = grab (as++[c],bs) ss
                                | q!!n == Up   = grab (as,bs++[c]) ss


-- applyGate an n number of times
applyGateN :: Qgate -> Int -> Qstate -> Qstate
applyGateN _    0 state = state
applyGateN gate n state = applyGateN gate (n-1) $ applyGate gate state


-- define a gate that works n times after a measurement
gateAfterMeas :: Qgate -> Int -> Qstate -> Qstate
gateAfterMeas gate n s@(State x) = applyGateN gate m s
              where m  = qbitToInt $ qs !! n -- convert measured qbit to integer
                    qs = snd $ x !! 0        -- grab the first state (doesn't matter since measured)

-- apply X gate to qbit after measurement
xgateM :: Int -> Int -> Qstate -> Qstate
xgateM target control state = gateAfterMeas (Xgate target) control state   

-- apply Z gate to qbit after measurement
zgateM :: Int -> Int -> Qstate -> Qstate
zgateM target control state = gateAfterMeas (Zgate target) control state   

-- apply H gate to qbit after measurement
hgateM :: Int -> Int -> Qstate -> Qstate
hgateM target control state = gateAfterMeas (Hgate target) control state                  

-- choose either Up or Down with r as random number
choose :: Float -> (Complex,Complex) -> Qbit
choose r (a,b)
   | r <= (complexToFloat a)     = Down
   | otherwise = Up



----------------------------
-- Shortcut gates applied --
----------------------------

xgate target   = applyGate (Xgate target)
zgate target   = applyGate (Zgate target)  
hgate target   = applyGate (Hgate target)
cnot  target c = applyGate (CNOT  target c) 
meas  target r = applyGate (Meas  target r)



----------------------
-- Helper Functions --
----------------------

-- replace nth value with a new value
replaceN n newVal (x:xs)
      | n == 0 = newVal:xs
      | otherwise = x:replaceN (n-1) newVal xs

-- split with Hgate
hSplit :: Int -> ClassicState -> [ClassicState]
hSplit n (a,qs) 
       |q == Up   = [ (a',replaceN n Down qs), (cNegate a',qs) ]
       |q == Down = [ (a',replaceN n Up   qs), (a', qs) ]
       where q  = qs !! n
             a' = a * (SqFrac 1 2) -- divide by sqrt of 2 a / (sqrt 2)      
















