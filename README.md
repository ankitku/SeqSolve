# SeqSolve
SeqSolve is a solver for deciding the satisfiability of string equations with length constraints. It is based on TranSeq, a highly non-deterministic, branching transition system. TranSeq is an extension of the Mathematical Programming Modulo Theories (MPMT) constraint solving framework and is designed to enable useful and computationally efficient inferences that reduce the search space, that encode certain string constraints and theory lemmas as integer linear constraints and that otherwise split problems into simpler cases, via branching. It uses [Z3](https://github.com/Z3Prover/z3) as a backend ILP solver to solve generated linear constraints.

SeqSolve is implemented in [ACL2s](http://acl2s.ccs.neu.edu) which allowed us to 
1. define complex datatypes like blocks, sequences and valid Z3 expressions (used in queries made to Z3)
2. encode TranSeq rules as typed and terminating functions over these datatypes and 
3. interface with Z3, using a Z3-ACL2s interfacing library

To get started, make sure that Z3 and ACL2s are installed and on $PATH.

Then, run ./setup.sh, to create builds in the build directory.
