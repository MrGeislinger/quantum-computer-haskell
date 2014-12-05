module QuantumComputer.TeleportEx
( psi
, psi0
, psi1
, psi2
, teleportGates
, teleportCircuit
) where

import QuantumComputer.ConstantStates
import QuantumComputer.Gates
import QuantumComputer.QStates

-- teleportation
psi  = qstate0
psi0 = psi `combineStates` bell00
psi1 = hgate 0 psi0
psi2 = cnot 0 1 psi1


-- same as above but all at once (right is done first) (id applied last)
teleportGates r0 r1 = foldl1 (.) [ zgateM 2 0, xgateM 2 1, meas 1 r1, meas 0 r0, hgate 0, cnot 1 0]

teleportCircuit r0 r1 state0 = teleportGates r0 r1 $ state0 `combineStates` bell00
