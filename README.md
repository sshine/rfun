rfun
====

A compiler for a subset of RFUN, a reversible functional language with algebraic
types.  It supports functions, the two datatype constructors Nil/0 and Cons/2,
giving way to construct binary trees, let-bindings and case-of expressions that
match only one level deep using one case (for correct programs) or two cases (in
either order).  RFUN's first-match policy is upheld by simplicity.

The subset is discussed in the article: ``Reversible representation and
manipulation of constructor terms in the heap'' by Holger Bock Axelsen and
Robert Glück, 2013.  The present compiler is an attempt at implementing most of
this subset.

The compiler currently skips type checking and emphasis has been put on code
generation.  The code generator targets PISA, described in ``The Pendulum
instruction set architecture'', an instruction set summary by Holger Bock
Axelsen from 2006.

As an aid, inspiration was drawn from the article, ``Clean translation of an
imperative reversible programming language'' by Holger Bock Axelsen, 2011, in
particular the overall program structure and calling convention code is copied,
and translation of conditionals has been repurposed.

Writing this compiler has been an experience in reversible machine code and
structuring a compiler from scratch using monad transformers.
