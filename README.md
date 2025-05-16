# isa-mu0
Instruction Set Architecture of MU0 written in haskell

### Definition
Instruction Set Architecture is a pure mathematical function defined as :

$$
\text{ISA}:\text{Instruction}\rightarrow \text{Maybe }(\text{State}->\text{State})
$$

This project is the proof itself of this statement.

> [!NOTE]
> The return value `Nothing` indicates STP or undefined instructions.

### Usage
Load ISA.hs file into ghci
```bash
ghci ISA.hs
```

Declare `MU0_State` with list of memory blocks $x$ by `mu0_getState` function.
```hs
let state = mu0_getState x
```

Declare infinite list of `MU0_State` with `mu0_getStateList`
```hs
let stateList = state >>= Just . mu0_getStateList
```
Each element represents `MU0_State` after execution of element before.


Use `readaccAt` function to read acc register of $n$-th element of list.
```hs
stateList >>= (readaccAt n) 
```

Use `readpcAt` function to read pc register of $n$-th element of list.
```hs
stateList >>= (readpcAt n)
```

Use `readWordAt` function to read $m$-th memory block of $n$-th element of list.
```hs
stateList >>= (readWordAt n m)
```
