module QuantumComputer.ConstantStates
( qstate0
, qstate1  
, bell0 
, bell1
, bell00
, bell01
, bell10
, bell11
) where  

import QuantumComputer.Gates
import QuantumComputer.QStates

---------------------------
-- Some Constants to Use --
---------------------------

-- basic single qbit states
qstate0 = State [(cOne,[Down])]
qstate1 = State [(cOne,[Up])]

-- split single qbit states (before bell state is formed)
bell0 = applyGate (Hgate 0) qstate0
bell1 = applyGate (Hgate 0) qstate1

-- bell states (2 qbits)
bell00 = applyGate (CNOT 1 0) $ bell0 `combineStates` qstate0
bell01 = applyGate (CNOT 1 0) $ bell0 `combineStates` qstate1
bell10 = applyGate (CNOT 1 0) $ bell1 `combineStates` qstate0
bell11 = applyGate (CNOT 1 0) $ bell1 `combineStates` qstate1


