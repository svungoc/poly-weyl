# Polynomials and Weyl algebra

Computer implementation of generic polynomials and semiclassical Weyl algebra

Work in progress...

[Documentation](https://svungoc.github.io/poly-weyl/index.html) is available.


## Module Math (package math-alg)

+ Basics algebraic structures:

	- groups
	- rings
	- fields
	- modules over a ring
	- vector spaces over a field
	- algebra over a ring
	- Lie algegra

+ Usual implementations

	- machine integers
	- arbitrary long integers (Zarith)
	- exact rationals (Zarith)
	- machine float ("real numbers")
	- complex numbers
	- complexification of an arbitrary field
	
+ Polynomials (over arbitrary rings)

	- arbitrary number of variables
	- special implementation for one variable
	- usual operations: addition, multiplication, integer power, derivation
	- tensor products
	- printing via conversion to LaTeX
	- display formula in browser or PDF (via the *Latex* module)

+ Semiclassical Weyl algebras

	- Canonical variables (q,p) and semiclassical parameter ħ
	- Poisson bracket
	- Moyal bracket
	- TODO: Birkhoff normal forms

## Module Latex (package math-latex)

Conversion to/from Latex code for simple algebraic expressions.

