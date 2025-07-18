# 🧠 Task 3: Turing Machine Embedded in Haskell

This repository contains an **embedded implementation** of a **deterministic Turing Machine (TM)** in Haskell. The core idea is to model all components of a classical Turing Machine—alphabet, tape, states, and transition function—using Haskell's native types and functional abstractions.

The code allows you to define various Turing Machines (as data structures) and execute them directly on given tapes, simulating their behavior step by step.

---

## 📘 What Is an Embedded Turing Machine?

An **embedded Turing Machine** is an implementation where the *host language* (Haskell, in this case) is used not only to run the simulation, but also to **define the machines themselves as values**. That is, instead of writing and parsing external descriptions or configurations, all the logic is embedded directly into Haskell expressions.

This approach takes full advantage of Haskell's **type system, recursion, and functional composition**, making it especially well-suited to elegantly represent the foundations of computation theory.

---

## 🧠 What Is a Turing Machine?

A **Turing Machine** is a mathematical model of computation introduced by **Alan Turing** in 1936. It formalizes the notion of an algorithm or mechanical computation through:

- A finite set of **states**
- A **tape** divided into cells, each containing a symbol from a defined **alphabet**
- A **read/write head** that can move left or right along the tape
- A **transition function** that dictates, based on the current state and tape symbol, what symbol to write, which direction to move, and what the next state should be

Despite its simplicity, the Turing Machine is capable of expressing any computation that can be algorithmically defined—making it a central model in theoretical computer science.

---

## 🧩 Code Structure

- `Σ` (Alphabet): Represented as `String`
- `Tape`: Modeled as a triple `(left, current, right)` of symbols
- `Q` (States): Each state is a `String`
- `M` (Machine): A list of `(state, transitions)` pairs
- `Action`: Can be `L` (Left), `R` (Right), or `W σ` (Write a symbol)
- `exec`: Runs a machine on an input tape starting from the initial state `i`, stopping at the halting state `h`

---

## 🛠 Example Machines

The repository includes four example machines defined using this framework:

### 1. `left_sigma`
Moves left until it finds a specified symbol `σ` and positions the head on it.

### 2. `par`
Checks whether the number of `I` symbols on the tape is even or odd, writing `True` or `False` accordingly.

### 3. `elem_sigma`
Checks if a specified symbol `σ` appears anywhere on the tape, and writes `True` or `False` at the end.

### 4. `reverse`
Reads a sequence of `a`s and `b`s before a `#` symbol, and writes a reversed copy of that sequence at the end of the tape.

---
