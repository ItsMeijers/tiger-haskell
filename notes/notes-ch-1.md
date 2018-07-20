# Notes on Chapter 1 - Introduction

Description of the compiler phases:

|Phase                  | Descriptions                                                                                                                                                                                             |
|-----------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Lex                    | Break the source file into individual words, or tokens.                                                                                                                                                  |
|Parse                  | Analyze the structure of the program.                                                                                                                                                                    |
|Semantic Actions       | Build a piece of abstract syntax tree corresponding to each phrase.                                                                                                                                      |
|Semantic Analysis      | Determine what each phrase means, relate uses of variables to their definitions, check types of expressions, request translation of each phrase.                                                         |
|Frame layout           | Place variables, functions-parameters, etc. into activation records (stack frames) in a machine-dependent way.                                                                                           |
|Canonicalize           | Produce intermediate representation trees (IR trees), a notation that is not tied to any particular source language or target-machine architecture.                                                      |
|Instruction Selection  | Group the IR-tree nodes into clumps that correspond to the actions of target-machine instructions                                                                                                        |
|Control Flow Analaysis | Analyze the sequence of instructions into a control flow graph that shows all the possible flows of control the program might follow when it executes.                                                   |
|Dataflow Analysis      | Gather information about the flow of information through variables of the program; for example, liveness analysis calculates the place where each program variable holds a still-needed value (is live). |
|Register Allocation    | Choose a register to hold each of the variables and temporary values sued by the program; variables not live at the same time can share the same register.                                               |
|Code Emission          | Replace the temporary names in each machine instruction with machine registers.                                                                                                                          |

A simple programming langauge with statements and expressions, but no loops or if-statements is called a language of _straight-line programs_.

Writing interpreters without side effects (that is, assignment statements that update variables and data structures
(not really?)) is a good introduction to _denotation symantics_ and _attribute grammars_, which are methods for
describing what programming languages do. It's often a useful technique in writing compilers, too; compilers are
also in the business of saying what programming languages do.

