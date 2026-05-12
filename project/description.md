# Finite-Field Algebra and Cryptographic Primitives in Haskell

## Motivation

Zero-knowledge proof systems — Groth16, Plonk, STARKs — are reshaping how blockchains verify computation: instead of re-executing every transaction, a prover produces a short cryptographic proof that a verifier can check in milliseconds. At the bottom of every such system sits the same algebraic machinery — arithmetic over prime fields, polynomial manipulation, and interactive proof protocols — combined in different ways. Production implementations live in Rust and C++ for performance, but the systems-level concerns (memory layout, borrow semantics, SIMD intrinsics) make the underlying mathematics hard to audit and hard to verify against the papers they implement. Haskell's typeclass system maps directly onto algebraic structures (fields, rings, polynomial rings), its purity turns protocols into values rather than side-effecting procedures, and property-based testing can express security guarantees — completeness, soundness — as executable checks. This project builds a general-purpose finite-field algebra library and uses it to implement the sumcheck protocol, the interactive proof at the core of modern proof systems like Lasso, Jolt, and Spartan.

## Project Overview

The project is a Haskell library in two layers. The first layer is a general-purpose algebraic toolkit: a typeclass hierarchy for fields, a prime-field implementation, univariate and multilinear polynomial arithmetic, and a number-theoretic transform (NTT) for fast polynomial multiplication. The second layer is a collection of cryptographic primitives built entirely on that algebraic foundation — centred on the sumcheck interactive proof protocol and its Fiat-Shamir non-interactive variant, with an architecture designed to grow toward further primitives (algebraic hashing, polynomial commitments, SNARKs). Together, the two layers form an executable specification of the algebraic core shared by all modern zero-knowledge proof systems — readable as mathematics, runnable as software, and testable against the security properties defined in the literature.

## Key Goals

1. **Algebraic Library**: A complete, reusable Haskell library for finite-field arithmetic and polynomial algebra, with a clean typeclass hierarchy and efficient NTT-based multiplication.
2. **Cryptographic Primitives**: A layer of cryptographic primitives built on the algebraic library, centred on the sumcheck interactive proof protocol (prover, verifier, Fiat-Shamir transform). The architecture is designed so that additional primitives — algebraic hash functions (MiMC, Poseidon), polynomial commitment schemes, or constraint systems (R1CS) — slot in as further modules consuming the same algebraic foundation.
3. **Test Suite**: Property-based tests for algebraic laws (field axioms, polynomial identities, NTT round-trips) and protocol security (completeness, soundness), plus unit and end-to-end tests.
4. **Additional Primitives (stretch)**: One or more further cryptographic primitives on top of the algebraic library — for example a Merkle-tree-based polynomial commitment scheme (toward a full STARK), an algebraic hash function like MiMC, or an R1CS constraint system with witness verification.

## Suggested Core Data Types

A starting point — adapt to your design.

```haskell
-- Algebraic hierarchy via typeclasses
class (Eq a, Show a, Num a) => Ring a

class Ring a => Field a where
  fInv :: a -> a                       -- multiplicative inverse

class Field a => FiniteField a where
  fieldOrder :: proxy a -> Integer     -- number of elements

-- Prime-field element: integers mod p
newtype Fp = Fp Integer                -- invariant: 0 <= val < p

-- Univariate polynomial over any field
newtype Poly f = Poly [f]              -- coefficients, index = degree

-- Multilinear polynomial over any field
-- Represented by its 2^n evaluations on the boolean hypercube {0,1}^n
data MultilinearPoly f = MLP
  { mlpNumVars :: Int
  , mlpEvals   :: [f]                  -- length 2^n
  }

-- Sumcheck protocol messages
data RoundMessage f = RoundMessage (Poly f)

data SumcheckProof f = SumcheckProof
  { proofRounds    :: [RoundMessage f]
  , proofFinalEval :: f
  }

data Verdict = Accept | Reject String
```

`Fp` is an instance of `Num` and `Fractional`, so polynomial code reads like ordinary arithmetic. `Poly` is also a `Num` instance — you can write `p * q + r` and get polynomial algebra. The typeclass hierarchy ensures that the sumcheck code is generic over any `Field`, not tied to a specific prime.

## Example

There is no parser — the library is used directly from Haskell. A typical session:

```haskell
-- Arithmetic over F_97
> let a = 42 :: Fp 97
> let b = 80 :: Fp 97
> a + b
25                          -- (42 + 80) mod 97
> a * fInv a
1                           -- multiplicative inverse works

-- Polynomial over F_97
> let p = poly [3, 1, 0, 1]   -- 3 + x + x^3
> polyEval p 5
34                          -- (3 + 5 + 125) mod 97

-- Lagrange interpolation round-trip
> let pts = [(i, polyEval p i) | i <- [0..3]]
> lagrange pts
Poly [3, 1, 0, 1]          -- recovers p exactly

-- NTT-based fast multiplication
> let q = poly [1, 1]         -- 1 + x
> polyMulNTT p q
Poly [3, 4, 1, 1, 1]       -- (3 + x + x^3)(1 + x) = 3 + 4x + x^2 + x^3 + x^4

-- Sumcheck: prove the sum of f over {0,1}^2
> let f = mlp 2 [10, 20, 30, 40]
>   -- f(0,0) = 10, f(0,1) = 20, f(1,0) = 30, f(1,1) = 40
> let claim = 100              -- 10 + 20 + 30 + 40

> let proof = sumcheckProve f claim
> sumcheckVerify 2 claim (mleEval f) proof
Accept

-- Wrong claim is rejected
> sumcheckVerify 2 99 (mleEval f) proof
Reject "Round 1: g(0) + g(1) = 100, expected 99"
```

## Implementation Components

### 1. Algebraic Library

- **Typeclass hierarchy**: `Ring` and `Field` classes encoding the algebraic laws. `Fp` as the concrete instance with `Num`, `Fractional`, and `Field` instances. The hierarchy should allow the protocol code to be written generically — `sumcheckProve :: Field f => ...` — so that swapping the underlying field requires no code changes.
- **Prime-field arithmetic**: Addition, multiplication, and subtraction are modular arithmetic. Multiplicative inverse via the extended Euclidean algorithm. Fast modular exponentiation by squaring.
- **Univariate polynomials**: Representation as coefficient lists. Evaluation via Horner's method. Addition, scalar multiplication, and naïve `O(n²)` multiplication. Polynomial long division producing quotient and remainder. Lagrange interpolation from a list of points.
- **Number-Theoretic Transform (NTT)**: The finite-field analogue of the FFT, using a primitive root of unity in `Fp` (choosing `p` such that a suitable root exists). Forward and inverse transforms, yielding `O(n log n)` polynomial multiplication. This replaces the naïve multiplication when polynomial degree is large.

### 2. Cryptographic Primitives

- **Multilinear polynomials**: Represent an `n`-variate multilinear polynomial by its `2^n` evaluations on `{0,1}^n`. Evaluate at an arbitrary point via the multilinear extension. Fix one variable to a field element, producing an `(n-1)`-variate polynomial. Compute partial sums over unfixed variables.
- **Prover**: Given a multilinear polynomial `f` and a claimed sum `H`, generate `n` round messages. Each round message is a univariate polynomial `gᵢ` obtained by summing `f` over the remaining boolean variables while leaving variable `i` free. The prover incorporates the verifier's challenges from previous rounds by fixing variables.
- **Verifier**: Check each round message: `gᵢ(0) + gᵢ(1)` must equal the previous round's value (or `H` for round 1). After all rounds, check the final evaluation against the polynomial directly. Return `Accept` or `Reject` with a diagnostic message.
- **Fiat-Shamir transform**: Make the protocol non-interactive by deriving each verifier challenge deterministically from a running hash transcript (the `State` monad over a transcript). This turns the interactive proof into a self-contained proof object that anyone can verify without interaction.
- **Extensible architecture**: The primitives layer consumes the algebraic library through its typeclass interface (`Field f => ...`), so additional primitives — algebraic hashing, polynomial commitments, constraint systems — can be added as new modules without modifying the existing code.

### 3. Test Suite

- **Unit tests**: prime-field arithmetic on hand-computed examples; polynomial evaluation, division, and interpolation on small cases; each sumcheck round checked against a manually worked example.
- **End-to-end tests**: construct a multilinear polynomial with a known sum, generate a proof, verify it — for two or three different polynomials and field sizes. Construct a proof with a wrong claim, verify that it is rejected.
- **Property-based tests**:
  - Field axioms: commutativity, associativity, distributivity, `a * fInv a == 1` for all nonzero `a`, `(a + b) * c == a*c + b*c` for random `a, b, c :: Fp`.
  - Polynomial identities: `polyEval (lagrange pts) xᵢ == yᵢ` for random point sets; `p == q * d + r` where `(q, r) = polyDiv p d`; `degree (p * q) == degree p + degree q`.
  - NTT round-trip: `invNtt (ntt xs) == xs` for random coefficient lists.
  - Completeness: for a random multilinear polynomial, `sumcheckVerify` accepts the proof produced by `sumcheckProve` with the correct sum.
  - Soundness: for a random multilinear polynomial and a random _incorrect_ sum, `sumcheckVerify` rejects.

## Submission

Commit the completed project to your personal course repository — the same repo you use for homework — in a `project/` folder next to the existing `homeworks/` folder.
