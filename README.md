# Parallel CDCL SAT Solver in Haskell

Welcome to **parallel-cdcl-sat-hs**, a **Conflict-Driven Clause Learning (CDCL)** based SAT solver written in **Haskell** with **parallel** search capabilities. This project aims to combine cutting-edge SAT solving techniques with Haskell’s robust concurrency model to deliver a highly efficient solver, showcasing the power of functional programming for advanced algorithmic challenges.

By leveraging multiple cores and sharing learned clauses among concurrent threads, **parallel-cdcl-sat-hs** aims to compete with and potentially outperform established C++ solvers like **Z3** or **MiniSat** in certain scenarios. 

> **Note**: Building a parallel SAT solver in Haskell that can beat top-tier C++ solvers is extremely ambitious. However, the unique design, thorough documentation, and demonstration of concurrent functional programming are intended to highlight both your engineering and research skills—ideally leading to **high-paying job offers** in fields like formal verification, software security, and functional programming.

---

## Table of Contents
1. [Motivation](#motivation)
2. [Features](#features)
3. [Getting Started](#getting-started)
   - [Prerequisites](#prerequisites)
   - [Installation](#installation)
   - [Building from Source](#building-from-source)
   - [Running the Solver](#running-the-solver)
4. [Usage](#usage)
5. [High-Level Overview](#high-level-overview)
   - [CDCL Algorithm](#cdcl-algorithm)
   - [Parallel Strategies](#parallel-strategies)
6. [Documentation](#documentation)
7. [License](#license)

---

## Motivation

Modern SAT solvers power a wide range of applications, from **formal verification**, **hardware/software testing**, to **cryptographic analysis**, and beyond. Many state-of-the-art solvers are implemented in low-level languages like C++ to squeeze every ounce of performance.  
**parallel-cdcl-sat-hs** demonstrates that:

- **Haskell** can achieve competitive performance when used with **proper concurrency** and **strict data structures**.  
- **Parallel approaches** to SAT solving (portfolio methods, search-space splitting) can significantly reduce solve times for large or complex Boolean formulas.  
- With comprehensive documentation and a well-structured codebase, others can learn, build upon, and contribute to advanced solver research in the functional programming community.

---

## Features

- **CDCL Core**: Implements standard Conflict-Driven Clause Learning with watched literals, backjumping, and advanced conflict analysis.
- **Parallel Search**: Option to run multiple solver instances (portfolio approach) or split the search space among multiple threads.
- **Clause Sharing**: Learnt clauses are broadcast between threads to prevent redundant work.
- **Modern Heuristics**: Features like **VSIDS** (Variable State Independent Decaying Sum) for variable selection; optional expansions to advanced heuristics.
- **Configurable Restarts**: Supports geometric and Luby restart policies.
- **Haskell Concurrency**: Takes advantage of GHC’s lightweight threads and STM (Software Transactional Memory) for efficient synchronization.
- **Extensive Documentation**: Clear explanations and tutorials for both end users and developers.

---

## Getting Started

### Prerequisites

- **GHC (Glasgow Haskell Compiler) 8.10+** recommended
- **Cabal** or **Stack** for build and dependency management
- A **multi-core CPU** to leverage parallelism
- Basic familiarity with command line usage

### Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/your-username/parallel-cdcl-sat-hs.git
   cd parallel-cdcl-sat-hs
   ```
   
2. **Install Dependencies** (using Cabal):
   ```bash
   cabal update
   cabal build
   ```
   Or (using Stack):
   ```bash
   stack setup
   stack build
   ```
### Building from Source

- **Cabal**:
  ```bash
  cabal configure --enable-optimization
  cabal build
  ```
- **Stack**:
  ```bash
  stack build --fast
  ```
- **Enabling Parallelism**:
  GHC’s runtime supports multithreading. Build with the -threaded flag (Cabal or Stack will typically handle this). When running, use +RTS -N to enable all cores.

### Running the Solver

After building, you’ll have an executable (e.g., parallel-cdcl-sat-hs).
   ```bash
   # Cabal
   cabal run parallel-cdcl-sat-hs -- --file path/to/formula.cnf --mode portfolio --threads 4

   # Stack
   stack run parallel-cdcl-sat-hs -- --file path/to/formula.cnf --mode split --threads 8
   ```
**CLI Options** (sample):
- --file FILE : Path to a CNF file (DIMACS format).
- --mode [single|portfolio|split] : Select the parallelization strategy.
- --threads N : Number of threads to spawn.
- --verbose : Enable verbose logging.
- --stats : Print solver statistics (e.g., conflicts, restarts, time).

---

## Usage

Here’s a minimal step-by-step usage example:
1. Obtain a Boolean formula in **DIMACS CNF** format (e.g., example.cnf).
2. Run the solver in single-threaded mode:
   ```bash
   parallel-cdcl-sat-hs --file example.cnf --mode single
   ```
3. For parallel solving with 4 threads:
   ```bash
   parallel-cdcl-sat-hs --file example.cnf --mode portfolio --threads 4
   ```
4. View stats:
   ```bash
   parallel-cdcl-sat-hs --file example.cnf --mode portfolio --threads 4 --stats
   ```
5. Observe learned clauses, conflicts, backtracks, and final result (SAT/UNSAT).

---

## High-Level Overview
### CDCL Algorithm
CDCL (Conflict-Driven Clause Learning) is the backbone of many modern SAT solvers. Key steps include:
1. **Decide**: Pick an unassigned variable and assign it True or False.
2. **Propagate**: Use the watched-literal scheme to propagate forced assignments until no new units arise or a conflict is found.
3. **Conflict Analysis**: If a conflict occurs, derive a **conflict clause**. Then **backjump** to the appropriate level.
4. **Learn**: Add the conflict clause to the database to prune future search paths.
5. **Restart**: Periodically restart the search with some learned clauses retained.
### Parallel Strategies
1. **Portfolio Approach**:
- Run multiple solver instances (each with distinct heuristics or random seeds).
- Occasionally share learned clauses, especially short ones.
- Whichever thread finds SAT or proves UNSAT terminates the entire run.
2. **Search-Space Splitting**:
- Split the set of variables (or partial assignments) among multiple threads.
- Each thread explores a different portion of the search space, sharing results and learned clauses as needed.

---

## License
This project is licensed under the MIT License. See the LICENSE file for details.







