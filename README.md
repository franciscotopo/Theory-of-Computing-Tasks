# Task 1: Functional Language with Weak Evaluation in Haskell

Task 2 is the implementation of an **embedded functional language in Haskell** that supports algebraic data types, pattern matching, recursion, and weak evaluation semantics. The language models core ideas from lambda calculus and functional programming within Haskell using its type system and functional constructs.

---

## 📘 What Is This?

This project defines:

- A **pure functional language** embedded in Haskell (`E`, `V`, `W`)
- Support for:
  - **Lambda abstraction** and application
  - **Algebraic data constructors**
  - **Pattern matching** via `Case`
  - **Recursive functions** via `Rec`
  - **Weak evaluation strategy**

It allows you to write and evaluate functional programs within Haskell using your own syntax tree.

---

## Language Syntax

### Expressions (`E`)
```haskell
data E = Var X                  -- Variable
       | Cons K [E]             -- Constructor application
       | Lamb X E               -- Lambda abstraction
       | Apl E E                -- Application
       | Case E [B]             -- Pattern matching
       | Rec X E                -- Recursive function



# Task 2: Embedded Imperative Language in Haskell

This repository implements an **embedded domain-specific language (DSL)** in Haskell that models a small **typed imperative programming language**. It supports variables, assignments, local scopes, conditionals, pattern matching (`case`), and recursive loops (`while`) using algebraic data types and functional evaluation. Basically, an imperative language called **IMP** inside **Haskell**

All programs are written **within Haskell itself**, as expressions of the embedded language.

---

## 📘 What Is This?

This is a **structural operational semantics interpreter** written in Haskell for a mini language with:

- **Expressions (`E`)**: Either variables or constructors applied to expressions.
- **Programs (`P`)**: Sequences of assignments, scopes, conditionals, and loops.
- **Values (`V`)**: Constructor values, e.g., `S 0`, or algebraic lists like `x : xs`.
- **Memory (`M`)**: A store mapping variables to values.

You can define programs and evaluate them directly on memory stores, simulating the execution of the language step by step.

---

## Language Constructs

The core abstract syntax tree is defined by:

```haskell
data P = [X] := [E]        -- Assignment
       | Local [X] P       -- Local variable scope
       | P :. P            -- Sequencing
       | Case X [B]        -- Pattern matching
       | While X [B]       -- Loop

---

# Task 3: Turing Machine Embedded in Haskell

Task 3 defines an **embedded implementation** of a **deterministic Turing Machine (TM)** in Haskell. The core idea is to model all components of a classical Turing Machine—alphabet, tape, states, and transition function—using Haskell's native types and functional abstractions.

The code allows you to define various Turing Machines (as data structures) and execute them directly on given **tapes**, simulating their behavior step by step.

---

## 📘 What Is a Turing Machine?

A **Turing Machine** is a mathematical model of computation introduced by **Alan Turing** in 1936. It formalizes the notion of an algorithm or mechanical computation through:

- A finite set of **states**
- A **tape** divided into cells, each containing a symbol from a defined **alphabet**
- A **read/write head** that can move left or right along the tape
- A **transition function** that dictates, based on the current state and tape symbol, what symbol to write, which direction to move, and what the next state should be

Despite its simplicity, the Turing Machine is capable of expressing any computation that can be algorithmically defined, making it a central model in theoretical computer science.

---

## 📘 What Is an Embedded Turing Machine?

An **embedded Turing Machine** is an implementation where the *host language* (Haskell, in this case) is used not only to run the simulation, but also to **define the machines themselves as values**. That is, instead of writing and parsing external descriptions or configurations, all the logic is embedded directly into Haskell expressions.

This approach takes full advantage of Haskell's **type system, recursion, and functional composition**, making it especially well-suited to elegantly represent the foundations of computation theory.

---

## Code Structure

- `Σ` (Alphabet): Represented as `String`
- `Tape`: Modeled as a triple `(left, current, right)` of symbols
- `Q` (States): Each state is a `String`
- `M` (Machine): A list of `(state, transitions)` pairs
- `Action`: Can be `L` (Left), `R` (Right), or `W σ` (Write a symbol)
- `exec`: Runs a machine on an input tape starting from the initial state `i`, stopping at the halting state `h`

---

## 🛠 Simple Examples Of Machines (Each machine would be a program)

### 1. `left_sigma`
Moves left until it finds a specified symbol `σ` and positions the head on it.

### 2. `par`
Checks whether the number of `I` symbols on the tape is even or odd, writing `True` or `False` accordingly.

### 3. `elem_sigma`
Checks if a specified symbol `σ` appears anywhere on the tape, and writes `True` or `False` if so.

### 4. `reverse`
Reads a sequence of characters before a, and writes a reversed copy of that sequence at the end of the tape.

---
