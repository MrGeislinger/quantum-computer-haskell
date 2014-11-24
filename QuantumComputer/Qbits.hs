module QuantumComputer.Qbits  
( Qbit(..)  
, flipQbit
, qbitToInt
) where  


-- make new qstate (single qbit) of superpositon of |0> and |1>
data Qbit = Up | Down deriving (Eq)
instance Show Qbit where       
    show (Up)   = "|1>"
    show (Down) = "|0>"


-- Flip a Qbit over 
flipQbit :: Qbit -> Qbit
flipQbit (Up)   = Down
flipQbit (Down) = Up

qbitToInt :: Qbit -> Int
qbitToInt Down = 0
qbitToInt Up   = 1














