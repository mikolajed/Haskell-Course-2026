# ZKAlgebra: Finite-Field Algebra and Cryptographic Primitives

## How to Test, Build, and Run

This project uses [Stack](https://docs.haskellstack.org/en/stable/README/) as its build tool.

To run the property-based, unit, and end-to-end test suite (112 tests across algebra, polynomials, sumcheck, Fiat-Shamir, and MiMC):

```bash
stack test
```

To build and execute the interactive demonstration of all the primitives working together:

```bash
stack run
```

---

## Understanding the `stack run` Output

When you execute `stack run`, the application steps through a series of increasingly complex mathematical and cryptographic concepts to prove they work in practice:

1. **Finite Fields ($F_{97}$ and $F_{251}$)**: Demonstrates basic field arithmetic (addition, multiplication, inverses, and Fermat's Little Theorem).
2. **Polynomial Arithmetic**: Shows univariate polynomial operations, including long division, Lagrange interpolation, and $O(n \log n)$ multiplication using the Number-Theoretic Transform (NTT).
3. **Multilinear Polynomials**: Constructs an MLE and evaluates it over a boolean hypercube, fixing variables to compute partial sums.
4. **Sumcheck Protocol (Interactive)**: Executes the Sumcheck protocol using a step-by-step coroutine prover and verifier.
5. **Fiat-Shamir Non-Interactive Sumcheck**: Demonstrates the Fiat-Shamir transform, which collapses the interactive sumcheck into a static, self-contained proof object using an algebraic hashing transcript.
6. **Dynamic MiMC Algebraic Hash**: Showcases a ZK-friendly hash function operating purely on field elements that dynamically adapts its permutation exponent based on the prime field.
7. **Inner Product Proof**: Uses Sumcheck to prove the dot product of two vectors in zero-knowledge.
8. **Zero-Knowledge Alice vs Bob Scenario**: A grand finale showing Alice generating a Fiat-Shamir proof of a dot-product claim, sending it to Bob (without revealing her secret vectors), and Bob successfully verifying it.

---

## Project Evaluation: Haskell as an Executable Specification

This project serves to answer a broader architectural question: **Can Haskell be used as a definition layer for cryptography to abstract away execution-level concerns?**

Zero-knowledge proof systems (like Plonk, STARKs, and Lasso) are mathematically incredibly dense. Production implementations of these protocols are almost exclusively written in systems languages like Rust or C++ (e.g., Arkworks, Halo2) to maximize performance. However, implementing complex cryptography while simultaneously battling a borrow checker, memory layouts, or SIMD intrinsics is a massive source of cognitive load—and inevitably, bugs.

This library demonstrates that Haskell's typeclass system and purity map naturally onto algebraic structures (fields, rings). By abstracting away systems-level execution details, Haskell allows developers to write code that closely mirrors the math papers they are implementing. But this abstraction is not free—it introduces its own tradeoffs, discussed in [Limitations & Tradeoffs](#limitations--tradeoffs) below.

### What Was Built

#### 1. Algebraic Library

- **Typeclass Hierarchy**: `Ring`, `Field`, `FiniteField` in [`ZKAlgebra.Algebra`](./src/ZKAlgebra/Algebra.hs).
- **Prime-field Arithmetic**: The `Fp p` type in [`ZKAlgebra.Field`](./src/ZKAlgebra/Field.hs) handles modular arithmetic, extended Euclidean algorithm inverses, and fast exponentiation.
- **Polynomials & NTT**: Univariate polynomials with $O(n \log n)$ multiplication using the Number-Theoretic Transform in [`ZKAlgebra.NTT`](./src/ZKAlgebra/NTT.hs).

#### 2. Cryptographic Primitives

- **Multilinear Polynomials**: Fully operational $n$-variate polynomials in [`ZKAlgebra.Multilinear`](./src/ZKAlgebra/Multilinear.hs).
- **Interactive Sumcheck**: Features a coroutine-driven `ProverState` in [`ZKAlgebra.Crypto.Sumcheck`](./src/ZKAlgebra/Crypto/Sumcheck.hs).
- **Fiat-Shamir Transform**: In [`ZKAlgebra.Crypto.FiatShamir`](./src/ZKAlgebra/Crypto/FiatShamir.hs). The transcript is 100% algebraic, absorbing field elements directly using the dynamic MiMC hash function and completely avoiding traditional byte-array serialization.

#### 3. Test Suite

- **112 tests** verify everything from algebraic laws (associativity, distributivity) to the **Completeness** and **Soundness** of both the Interactive and Fiat-Shamir Sumcheck protocols.

#### 4. Stretch Goal: Dynamic MiMC Algebraic Hash

- Implements the MiMC block cipher and Miyaguchi-Preneel hashing mode in [`ZKAlgebra.Crypto.MiMC`](./src/ZKAlgebra/Crypto/MiMC.hs). The hash function queries the type-level field characteristic $p$ at runtime and automatically selects the lowest secure permutation exponent $d \in \{3, 5, 7, 11\}$ such that $\gcd(d, p-1) = 1$.

---

## Design Decisions & Architecture

### 1. The Coroutine Prover: Security by Construction

The most important design decision in the project is the `ProverState` type in [`Sumcheck.hs`](./src/ZKAlgebra/Crypto/Sumcheck.hs):

```haskell
data ProverState f
  = ProverDone f
  | ProverRound (RoundMessage f) (f -> ProverState f)
```

This is not just a convenient encoding—it enforces a critical **security invariant at the type level**. In the interactive sumcheck protocol, the prover must compute round $j$'s polynomial *before* seeing the verifier's challenge $r_j$. A cheating prover that could peek at future challenges could forge proofs trivially.

Because the continuation `(f -> ProverState f)` is a function, it is structurally impossible for the code computing round $j$'s polynomial to inspect the challenge $r_j$—the challenge does not exist yet. The prover is forced to commit to a `RoundMessage` and then *suspend*, handing control back to the verifier.

In Rust, achieving this same guarantee requires careful API design with trait objects or closures and runtime discipline. In Haskell, it falls out naturally from the algebraic data type definition—the type system makes the insecure construction unrepresentable.

### 2. Separating Hypercube Geometry from Algebra

The multilinear polynomial module evolved through a deliberate refactoring. The original implementation of `mlePartialSumProduct` mixed two concerns: the *geometry* of slicing the boolean hypercube (which entry pairs correspond to $x_1 = 0$ vs $x_1 = 1$) and the *algebra* of interpolation and summation.

We separated these into two focused abstractions:

```haskell
-- Geometry: how the evaluation vector maps to variable assignments
activePairs :: Vector f -> [(f, f)]

-- Algebra: the multilinear interpolation formula
interpolatePair :: (Field f) => (f, f) -> f -> f
```

This separation of concerns makes the core sumcheck logic `mlePartialSumProduct` radically simpler. The `activePairs` function handles the combinatorial challenge of matching $f(0, x_2, \dots)$ with $f(1, x_2, \dots)$, leaving the algebraic logic clean and easy to verify against the math.

Crucially, this separation is exactly what allows the Haskell implementation to avoid the low-level array indexing and bit-twiddling (like `b << 1`) seen in systems-level implementations, cleanly decoupling the traversal of the boolean hypercube from the field arithmetic.

The result is that `mlePartialSumProduct` reads as pure mathematical intent:

```haskell
mlePartialSumProduct (MLP _ evalsA) (MLP _ evalsB) = lagrange [(0, g0), (1, g1), (2, g2)]
  where
    pairsA = activePairs evalsA
    pairsB = activePairs evalsB
    sumAt t =
      sum
        [ interpolatePair pA t * interpolatePair pB t
        | (pA, pB) <- zip pairsA pairsB
        ]
    g0 = sumAt 0
    g1 = sumAt 1
    g2 = sumAt 2
```

This separation was only natural because Haskell's laziness and garbage collection made it trivial to introduce the intermediate `[(f, f)]` list without worrying about allocation lifetimes. In Rust, introducing an equivalent intermediate allocation would require explicit lifetime annotations or cloning, discouraging this kind of clean separation.

### 3. The State Monad for Fiat-Shamir

The Fiat-Shamir transform requires maintaining a running transcript state. In imperative languages, this is typically a mutable struct passed by reference. In Haskell, the `State` monad threads the transcript invisibly:

```haskell
driveProverFS prover =
  evalState (go prover []) emptyTranscript
  where
    go (ProverDone v) acc = return $ SumcheckProof (reverse acc) v
    go (ProverRound msg k) acc = do
      appendToTranscript (polyCoeffs (roundPoly msg))
      r <- challenge
      go (k r) (msg : acc)
```

The critical insight: `appendToTranscript` and `challenge` look like side-effecting operations, but they are pure functions over a state value. The entire proof generation is deterministic and referentially transparent—given the same polynomial and claimed sum, `driveProverFS` always produces the exact same proof. This property is tested by QuickCheck (`same input -> same proof`, 100 random tests).

### 4. Dynamic MiMC Exponent Selection

The MiMC hash function's permutation $f(x) = x^d$ is only a valid block cipher if $\gcd(d, p-1) = 1$. Rather than hardcoding a specific exponent and hoping it works for all primes, the implementation queries the `FiniteField` typeclass at runtime:

```haskell
mimcExponent :: Integer -> Integer
mimcExponent p
  | gcd 3 (p - 1) == 1 = 3
  | gcd 5 (p - 1) == 1 = 5
  | gcd 7 (p - 1) == 1 = 7
  | gcd 11 (p - 1) == 1 = 11
  | otherwise = error "Could not find a small MiMC exponent for this prime"
```

This selects the lowest possible exponent to minimize the number of multiplication constraints in a ZK circuit. For $F_{251}$, the library selects $d = 3$ (since $\gcd(3, 250) = 1$), while for $F_{97}$, it selects $d = 5$ (since $\gcd(3, 96) = 3 \neq 1$, but $\gcd(5, 96) = 1$). The `FiniteField` constraint propagates cleanly through the Fiat-Shamir module without requiring any changes to the protocol logic.

---

## Deep Dive: Math from Paper to Haskell vs. Rust

To evaluate whether the project achieves its goal as a definition layer, this section compares how the math translates into Haskell versus a production systems language like Rust (specifically looking at paradigms from [Arkworks](https://github.com/arkworks-rs/sumcheck)).

### 1. The Sumcheck Prover: Protocol as Recursion

In the academic literature (specifically Section 3.2 of the [*Libra* paper by Xie et al. (CRYPTO 2019)](https://eprint.iacr.org/2019/317.pdf)), the sumcheck prover for the product of multilinear polynomials $a$ and $b$ (evaluating $\sum_{x \in \{0,1\}^n} a(x) \cdot b(x)$) is defined as an $n$-round interactive protocol:

- **Round $j$**: The prover computes $g_j(X_j) = \sum_{x_{j+1}, \dots, x_n \in \{0,1\}} a(r_1, \dots, r_{j-1}, X_j, \dots) \cdot b(\dots)$. They send $g_j$ to the verifier, and receive a random challenge $r_j$.
- **Final Step**: Once all $n$ challenges $(r_1, \dots, r_n)$ are drawn, the prover's claim is checked against the actual evaluations at $a(r_1, \dots, r_n) \cdot b(r_1, \dots, r_n)$.

#### The Haskell Implementation

See the actual code in [`src/ZKAlgebra/Crypto/Sumcheck.hs`](./src/ZKAlgebra/Crypto/Sumcheck.hs):

```haskell
sumcheckProverProduct a b
  | mlpNumVars a == 0 = ProverDone (mleEval a [] * mleEval b [])
  | otherwise =
      let g_i = mlePartialSumProduct a b
       in ProverRound (RoundMessage g_i) $ \r ->
            sumcheckProverProduct (mleFix a r) (mleFix b r)
```

This is six lines of executable code. Because it is written in a functional language, the $n$-round interactive protocol translates directly into structural recursion:

| Haskell | Mathematical Protocol |
|:--------|:----------------------|
| `mlpNumVars a == 0` | Final step: all $n$ rounds complete |
| `ProverDone (mleEval a [] * mleEval b [])` | Evaluate $a(r_1, \dots, r_n) \cdot b(r_1, \dots, r_n)$ |
| `mlePartialSumProduct a b` | Compute polynomial $g_j(X_j)$ |
| `ProverRound (RoundMessage g_i)` | Send $g_j$ to the verifier |
| `$ \r ->` | Receive challenge $r_j$ |
| `sumcheckProverProduct (mleFix a r) (mleFix b r)` | Fix $X_j = r_j$, proceed to round $j+1$ |

Crucially, the `ProverState` type (see [Design Decisions §1](#1-the-coroutine-prover-security-by-construction)) enforces a security invariant: the continuation `(f -> ProverState f)` makes it structurally impossible for round $j$'s computation to inspect challenge $r_j$—the challenge literally does not exist until the verifier provides it.

#### The Rust Contrast (Arkworks Paradigm)

In Rust, the same protocol is expressed as a mutable state machine. The actual [Arkworks sumcheck prover](https://github.com/arkworks-rs/sumcheck/blob/master/src/ml_sumcheck/protocol/prover.rs) manages state using a `ProverState` struct and manual round tracking:

```rust
// Actual code from arkworks-rs/sumcheck/src/ml_sumcheck/protocol/prover.rs
pub struct ProverState<F: Field> {
    pub randomness: Vec<F>,
    pub list_of_products: Vec<(F, Vec<usize>)>,
    pub flattened_ml_extensions: Vec<DenseMultilinearExtension<F>>,
    pub num_vars: usize,
    pub max_multiplicands: usize,
    pub round: usize,
}

pub fn prove_round(
    prover_state: &mut ProverState<F>,
    v_msg: &Option<VerifierMsg<F>>,
) -> ProverMsg<F> {
    if let Some(msg) = v_msg {
        prover_state.randomness.push(msg.randomness);
        // ... fix argument in flattened_ml_extensions
    }
    prover_state.round += 1;
    // ... evaluates polynomial for this round
}
```

The mathematical structure—the inductive case distinction, the suspension after each round—is obscured by:
1. **Mutable state management**: `prover_state.round += 1` and `prover_state.randomness.push(msg.randomness)` mutate the state in-place.
2. **Manual round tracking**: A manual `round` counter determines the control flow, rather than structural recursion that naturally terminates when variables are exhausted.
3. **No structural security guarantee**: Nothing in the Rust type system prevents `prove_round` from reading `prover_state.randomness` (past challenges) or any other mutable field incorrectly. Correctness depends on the developer's discipline, not structural enforcement by the type system.

### 2. The Fiat-Shamir Transcript

In the academic literature (introduced by [Fiat and Shamir, CRYPTO 1986](https://link.springer.com/chapter/10.1007/3-540-47721-7_12)), the Fiat-Shamir heuristic is described as a simple recurrence to generate non-interactive challenges:
$$ r_j = \text{Hash}( \text{Transcript}_{j-1} \ || \ g_j ) $$

#### The Haskell Implementation

See the actual Fiat-Shamir execution in [`src/ZKAlgebra/Crypto/FiatShamir.hs`](./src/ZKAlgebra/Crypto/FiatShamir.hs):

```haskell
driveProverFS ::
  forall p.
  (KnownNat p) =>
  ProverState (Fp p) ->
  SumcheckProof (Fp p)
driveProverFS prover =
  evalState (go prover []) emptyTranscript
  where
    go :: ProverState (Fp p) -> [RoundMessage (Fp p)] -> State (Transcript (Fp p)) (SumcheckProof (Fp p))
    go (ProverDone v) acc = return $ SumcheckProof (reverse acc) v
    go (ProverRound msg k) acc = do
      appendToTranscript (polyCoeffs (roundPoly msg))
      r <- challenge
      go (k r) (msg : acc)
```
The state is threaded invisibly. There are no mutable references. `appendToTranscript` and `challenge` are just stateful actions that the compiler evaluates purely.

#### The Rust Contrast (Arkworks Sponge Paradigm)

The core `arkworks-rs/sumcheck` repository does not actually implement Fiat-Shamir itself—it only implements the interactive rounds, forcing developers to build the Fiat-Shamir transform themselves using the `ark-crypto-primitives` sponge trait or a third-party transcript like `Merlin`.

When Arkworks developers build this, a transcript is a stateful struct. To use it, they must pass a mutable reference `&mut impl CryptographicSponge` (or `&mut Transcript`) to every single function that needs to derive a challenge (as seen in this [actual code from arkworks-rs/poly-commit](https://github.com/arkworks-rs/poly-commit/blob/master/poly-commit/src/marlin/mod.rs)):

```rust
// Actual code from arkworks-rs/poly-commit/src/marlin/mod.rs
fn accumulate_commitments_and_values<'a>(
    commitments: impl IntoIterator<Item = &'a LabeledCommitment<marlin_pc::Commitment<E>>>,
    values: impl IntoIterator<Item = E::ScalarField>,
    sponge: &mut impl CryptographicSponge,
    vk: Option<&marlin_pc::VerifierKey<E>>,
) -> Result<(E::G1, E::ScalarField), Error> {
    let mut combined_comm = E::G1::zero();
    let mut combined_value = E::ScalarField::zero();
    
    for (labeled_commitment, value) in commitments.into_iter().zip(values) {
        let challenge_i = sponge.squeeze_field_elements_with_sizes(&[CHALLENGE_SIZE])[0];
        // ... Use challenge to accumulate commitments and values ...
        combined_comm += &commitment.comm.0.mul(challenge_i);
        combined_value += &(value * &challenge_i);
    }
    // ...
}
```
This requirement to pass `&mut sponge` everywhere leads to borrow-checker conflicts if the sponge needs to be accessed concurrently, or if the prover needs to hold references to the sponge across asynchronous boundaries (which is common in distributed zero-knowledge provers). 

### 3. The Sumcheck Verifier: Pattern Matching as Protocol Logic

In the same section of the [*Libra* paper by Xie et al. (CRYPTO 2019)](https://eprint.iacr.org/2019/317.pdf), the sumcheck verifier performs a simple inductive check:

- **Round $j$**: Verify $g_j(0) + g_j(1) = H_j$. If it holds, set $H_{j+1} = g_j(r_j)$ and continue. Otherwise reject.
- **Final check**: Verify $H_n = f(r_1, \dots, r_n)$ via the oracle.

#### The Haskell Implementation

See the core verification loop in [`src/ZKAlgebra/Crypto/Sumcheck.hs`](./src/ZKAlgebra/Crypto/Sumcheck.hs):

```haskell
go expectedSum (r : rs) (RoundMessage g_i : rounds) =
  let g_sum = polyEval g_i 0 + polyEval g_i 1
   in if g_sum /= expectedSum
        then Reject $ "Round " ++ ...
        else go (polyEval g_i r) rs rounds
```

The Haskell pattern match `(r : rs) (RoundMessage g_i : rounds)` destructures one round at a time—exactly as the paper processes round $j$. The verification equation `polyEval g_i 0 + polyEval g_i 1` is a direct transcription of $g_j(0) + g_j(1) = H_j$. The recursive call `go (polyEval g_i r) rs rounds` sets $H_{j+1} = g_j(r_j)$ and advances. There are no loop counters, no index variables, and no mutable accumulators.

#### The Rust Contrast

In the actual [Arkworks verifier](https://github.com/arkworks-rs/sumcheck/blob/master/src/ml_sumcheck/protocol/verifier.rs), the same logic is expressed as a for-loop with a mutable accumulator:

```rust
// Actual code from arkworks-rs/sumcheck/src/ml_sumcheck/protocol/verifier.rs
pub fn check_and_generate_subclaim(
    verifier_state: VerifierState<F>,
    asserted_sum: F,
) -> Result<SubClaim<F>, crate::Error> {
    let mut expected = asserted_sum;

    for i in 0..verifier_state.nv {
        let evaluations = &verifier_state.polynomials_received[i];
        let p0 = evaluations[0];
        let p1 = evaluations[1];
        if p0 + p1 != expected {
            return Err(crate::Error::Reject(Some(
                "Prover message is not consistent with the claim.".into(),
            )));
        }
        expected = interpolate_uni_poly(evaluations, verifier_state.randomness[i]);
    }
    // ... returns expected SubClaim
}
```

The differences are subtle but meaningful for auditing:
1. **Mutable accumulator**: `expected` is reassigned each iteration, so auditing correctness requires tracing state changes across loop iterations. In Haskell, each recursive call is pure and self-contained.
2. **Implicit termination**: The Rust `for` loop silently bounds by `verifier_state.nv`. The Haskell version pattern-matches on `[] []`, making the structural base case explicitly visible.
3. **Error context boilerplate**: Haskell's `Verdict` sum type provides structured rejection reasons naturally. The Rust version uses `Result<_, crate::Error>`, adding boilerplate to surface the rejection string.

---

## Limitations & Tradeoffs

Using Haskell as a definition layer is not without cost. The following limitations were encountered during development and are inherent to the approach.

### 1. Laziness Requires Discipline

Haskell's default lazy evaluation can silently accumulate unevaluated thunks. In the Fiat-Shamir transcript, using `modify` (lazy) instead of `modify'` (strict) for `appendToTranscript` would build up a chain of deferred hash computations that only collapse when the final challenge is squeezed. For our small test polynomials this is harmless, but for a real prover handling $2^{20}$ variables it would cause a stack overflow.

The same applies to `mleSum`, where `V.foldl'` (strict left fold) was used instead of `V.foldl` to prevent thunk accumulation. This is a well-known Haskell pitfall, but it means the developer must consciously think about evaluation strategy in performance-sensitive code—precisely the kind of "execution-level concern" the project claims to abstract away.

### 2. No Compile-Time Primality Check

The `Fp p` type accepts any `KnownNat p` at the type level. Writing `Fp 100` compiles and runs, but produces algebraically meaningless results because 100 is not prime. There is no compile-time or runtime check that $p$ is prime. A production library would either use a type-level primality constraint (possible but complex in Haskell via type families) or at minimum a runtime assertion in `mkFp`.

### 3. Intermediate Allocations

The `activePairs` function returns a `[(f, f)]` list, which means every call to `mlePartialSumProduct` allocates a fresh list of pairs. For our test cases ($n \leq 3$, so at most 8 evaluations), this is negligible. For a real ZK prover with $n = 20$ ($2^{19}$ pairs), this allocation would dominate runtime. A production implementation would use `Data.Vector` with in-place strided access, or fuse the pairs into the summation loop—sacrificing exactly the clean separation that makes the Haskell version readable.

This is the fundamental tradeoff: the abstractions that make the code mirror the math (`activePairs`, `interpolatePair`, list comprehensions) are the same abstractions that prevent competitive performance.

### 4. The `Num` Typeclass Hierarchy Limitation

The algebraic hierarchy defines a `Group` typeclass, but no type in the library is an instance of it. This highlights a well-known limitation in Haskell's standard Prelude: the `Num` typeclass is mathematically imprecise. It bundles addition, multiplication, and `fromInteger` together. 

In a pure mathematical hierarchy, `Fp p` would first implement `Group` (for addition), then `Ring` (for multiplication), then `Field`. However, to use ergonomic operators like `(+)` and `(*)`, we were forced to implement `Num`. Because `Num` already provides the additive group operations, our custom `Group` typeclass became redundant. We traded mathematical purity in our typeclass hierarchy for developer ergonomics.

---

## Conclusion

This project demonstrates that Haskell is an effective language for *defining* and *auditing* cryptographic protocols. The `sumcheckProverProduct` function is six lines of code that directly mirror the paper's inductive protocol definition; the equivalent Arkworks pattern requires a mutable state struct with manual round counters and provides no structural guarantee against challenge inspection. The `driveProverFS` Fiat-Shamir driver threads transcript state invisibly through a `State` monad, while the equivalent Rust pattern requires explicit `&mut` references threaded through every function call. The `sumcheckVerify` verifier uses pattern matching to destructure rounds one at a time, making each verification step self-contained and auditable against the paper.

At the same time, the project reveals where this abstraction breaks down. Laziness forces explicit strictness annotations in accumulating code paths. Intermediate allocations that enable clean abstractions (`activePairs`, `interpolatePair`) would need to be fused away for production-scale inputs. And the type system, while powerful enough to encode field arithmetic generically, does not prevent the construction of algebraically invalid types like `Fp 100`.

The honest conclusion is not that Haskell is universally superior for cryptography, but that it occupies a specific and valuable niche: as a **reference specification** that can be property-tested against completeness and soundness guarantees (112 tests, including QuickCheck properties for both), audited line-by-line against the academic papers, and then used as a ground truth when implementing the optimized Rust or C++ version that will actually run in production.
