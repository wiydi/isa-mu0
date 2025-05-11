# isa-mu0
Instruction Set Architecture of MU0 written in haskell

### Definition
Instruction Set Architecture is a pure mathematical function defined as :

$$
\text{ISA}:\text{Instruction}\rightarrow \text{Maybe}\;(\text{State}->\text{State})
$$

This project is the proof itself of this statement.

> [!NOTE]
> The return value `Nothing` indicates STP or undefined instructions.