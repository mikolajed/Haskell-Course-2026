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
6. **Dynamic MiMC Algebraic Hash**: Showcases our stretch goal: a ZK-friendly hash function operating purely on field elements that dynamically adapts its permutation exponent based on the prime field.
7. **Inner Product Proof**: Uses Sumcheck to prove the dot product of two vectors in zero-knowledge.
8. **Zero-Knowledge Alice vs Bob Scenario**: A grand finale showing Alice generating a Fiat-Shamir proof of a dot-product claim, sending it to Bob (without revealing her secret vectors), and Bob successfully verifying it.

---

## Project Evaluation: Haskell as an Executable Specification

This project serves to answer a broader architectural question: **Can Haskell be used as a definition layer for cryptography to abstract away execution-level concerns?**

Zero-knowledge proof systems (like Plonk, STARKs, and Lasso) are mathematically incredibly dense. Production implementations of these protocols are almost exclusively written in systems languages like Rust or C++ (e.g., Arkworks, Halo2) to maximize performance. However, implementing complex cryptography while simultaneously battling a borrow checker, memory layouts, or SIMD intrinsics is a massive source of cognitive load—and inevitably, bugs.

This library successfully proves that Haskell's typeclass system and purity map perfectly onto algebraic structures (fields, rings). By abstracting away systems-level execution details, Haskell allows developers to write code that looks almost identical to the math papers they are implementing.

### Extensive Evaluation of Project Components

#### 1. Algebraic Library

- **Typeclass Hierarchy**: Provides a robust algebraic hierarchy (`Ring`, `Field`, `FiniteField`) in `ZKAlgebra.Algebra`.
- **Prime-field Arithmetic**: The `Fp p` type natively handles modular arithmetic, extended Euclidean algorithm inverses, and fast exponentiation.
- **Polynomials & NTT**: Univariate polynomials are implemented alongside $O(n \log n)$ polynomial multiplication using the Number-Theoretic Transform (NTT).

#### 2. Cryptographic Primitives

- **Multilinear Polynomials**: Fully operational $n$-variate polynomials.
- **Interactive Sumcheck**: Features a coroutine-driven `ProverState` that interacts with a verifier to prove the sum of a multilinear polynomial over a boolean hypercube.
- **Fiat-Shamir Transform**: Implemented in `ZKAlgebra.Crypto.FiatShamir`. **The Fiat-Shamir transcript is 100% algebraic**, absorbing field elements directly using the dynamic MiMC hash function and completely avoiding traditional byte-array serialization.

#### 3. Test Suite

- **112 tests** verify everything from algebraic laws (associativity, distributivity) to the **Completeness** and **Soundness** of both the Interactive and Fiat-Shamir Sumcheck protocols against adversarial provers.

#### 4. Stretch Goal: Additional Primitives

- **MiMC Algebraic Hash**: Implements the MiMC block cipher and Miyaguchi-Preneel hashing mode. The hash function is completely dynamic: it queries the type-level field characteristic $p$ at runtime and automatically selects the lowest secure permutation exponent $d \in \{3, 5, 7, 11\}$ such that $\gcd(d, p-1) = 1$.

---

## Deep Dive: Math from Paper to Haskell vs. Rust

To truly evaluate the success of this project as a definition layer, let's compare how the math translates into Haskell versus a production systems language like Rust (specifically looking at paradigms from [Arkworks](https://github.com/arkworks-rs/sumcheck)).

### 1. The Sumcheck Prover Polynomial

In the Sumcheck paper, the prover must compute a univariate polynomial $g_j(X_j)$ for round $j$ by fixing the first $j-1$ variables to the verifier's challenges $(r_1, \dots, r_{j-1})$, leaving $X_j$ free, and summing over all possible boolean combinations of the remaining variables:

$$ g_j(X_j) = \sum_{x_{j+1}, \dots, x_n \in \{0,1\}} f(r_1, \dots, r_{j-1}, X_j, x_{j+1}, \dots, x_n) $$

#### The Haskell Implementation

In our Haskell implementation, we can express this exact summation by evaluating the polynomial at $t \in \{0, 1, 2\}$ and using Lagrange interpolation. The list comprehension handles the hypercube cleanly without memory management overhead.

See the actual code in [`src/ZKAlgebra/Multilinear.hs`](./src/ZKAlgebra/Multilinear.hs):

```haskell
mlePartialSumProduct :: (Field f) => MultilinearPoly f -> MultilinearPoly f -> Poly f
mlePartialSumProduct (MLP _ evalsA) (MLP _ evalsB) = lagrange [(0, g0), (1, g1), (2, g2)]
  where
    halfSize = V.length evalsA `div` 2
    sumAt t =
      sum
        [ let a0 = evalsA V.! (2 * i)
              a1 = evalsA V.! (2 * i + 1)
              b0 = evalsB V.! (2 * i)
              b1 = evalsB V.! (2 * i + 1)
              aVal = a0 * (1 - t) + a1 * t
              bVal = b0 * (1 - t) + b1 * t
           in aVal * bVal
          | i <- [0 .. halfSize - 1]
        ]
    g0 = sumAt 0
    g1 = sumAt 1
    g2 = sumAt 2
```

The cognitive load is incredibly low. The code is a pure translation of the mathematical sigma $\sum$ combined with the linear interpolation formula $(1-t)x_0 + t x_1$. Because Haskell is lazy and pure, we do not worry about memory allocation for intermediate polynomials. The list comprehension maps over the boolean hypercube without any mutable state.

#### The Rust Contrast (Actual Arkworks Implementation)

In a production Rust implementation like Arkworks, implementing this exact same mathematical formula introduces extreme systems-level complexity. The developer must manage memory allocations, avoid cloning massive polynomial vectors, satisfy the borrow checker, and handle parallel iterators just to compute the sum. 

Here is the **exact, verbatim code** from the official [Arkworks sumcheck prover repository](https://github.com/arkworks-rs/sumcheck/blob/master/src/ml_sumcheck/protocol/prover.rs) that computes this sum:

```rust
// Actual code from arkworks-rs/sumcheck/src/ml_sumcheck/protocol/prover.rs
let fold_result = ark_std::cfg_into_iter!(0..1 << (nv - i), 1 << 10).fold(
    zeros,
    |(mut products_sum, mut product), b| {
        for (coefficient, products) in &prover_state.list_of_products {
            product.fill(*coefficient);
            for &jth_product in products {
                let table = &prover_state.flattened_ml_extensions[jth_product];
                let mut start = table[b << 1];
                let step = table[(b << 1) + 1] - start;
                for p in product.iter_mut() {
                    *p *= start;
                    start += step;
                }
            }
            for t in 0..degree + 1 {
                products_sum[t] += product[t];
            }
        }
        (products_sum, product)
    },
);

#[cfg(feature = "parallel")]
let products_sum = fold_result.map(|scratch| scratch.0).reduce(
    || vec![F::zero(); degree + 1],
    |mut overall_products_sum, sublist_sum| {
        overall_products_sum
            .iter_mut()
            .zip(sublist_sum.iter())
            .for_each(|(f, s)| *f += s);
        overall_products_sum
    },
);
```

In Rust, what was a single list comprehension in Haskell turns into:
1. **Manual Bit-twiddling**: Evaluating over the boolean hypercube requires tracking array indices via `b << 1` and `(b << 1) + 1`.
2. **Mutable Accumulators & Zipping**: Accumulating the sum requires mutability `products_sum[t] += product[t]`, breaking the pure mathematical paradigm. Reducing parallel chunks requires explicit `.iter_mut().zip().for_each()`.
3. **Macro Overload (`ark_std::cfg_into_iter!`)**: Handling multithreading (Rayon vs serial) requires compiler macros that leak thread-management logic directly into the mathematical definitions.

### 2. The Fiat-Shamir Transcript

Another prime example is the Fiat-Shamir heuristic, which requires maintaining a cryptographic transcript. 

In the academic papers, Fiat-Shamir is described as a simple recurrence:
$$ r_j = \text{Hash}( \text{Transcript}_{j-1} \ || \ g_j ) $$

#### The Haskell Implementation

In our Haskell project, we used the `State` monad to perfectly encapsulate this without ever exposing mutable state to the protocol logic. 

See the actual Fiat-Shamir execution in [`src/ZKAlgebra/Crypto/FiatShamir.hs`](./src/ZKAlgebra/Crypto/FiatShamir.hs):

```haskell
-- | Internal helper: drive any 'ProverState' to completion using Fiat-Shamir.
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
      -- Append the round polynomial coefficients directly to the transcript!
      appendToTranscript (polyCoeffs (roundPoly msg))
      -- Derive the challenge algebraically from the transcript
      r <- challenge
      -- Continue with the next round
      go (k r) (msg : acc)
```
The state is threaded invisibly. There are no mutable references. `appendToTranscript` and `challenge` are just stateful actions that the compiler evaluates purely.

#### The Rust Contrast (Arkworks Sponge Paradigm)

Unlike the `compute_gj` snippet above, this next Rust example is an illustrative synthesis. Why? Because the core `arkworks-rs/sumcheck` repository does not actually implement Fiat-Shamir itself—it only implements the interactive rounds, forcing developers to build the Fiat-Shamir transform themselves using the `ark_sponge` trait (or the `Merlin` transcript). 

When Arkworks developers build this, a transcript is a stateful struct. To use it, they are forced to pass a mutable reference `&mut impl CryptographicSponge` (or `&mut Transcript`) to every single function that needs to derive a challenge:

```rust
// An illustrative example of how Arkworks developers must implement Fiat-Shamir
pub fn prove_fs<F: PrimeField>(
    prover: &mut ProverState<F>,
    sponge: &mut impl CryptographicSponge<F>
) -> SumcheckProof<F> {
    let mut messages = Vec::new();
    
    while let Some(msg) = prover.next_round() {
        // Mutably borrow the sponge to absorb the message
        sponge.absorb(&msg.poly);
        
        // Mutably borrow it again to squeeze out the challenge
        let r = sponge.squeeze_field_elements(1)[0];
        
        prover.bind_challenge(r);
        messages.push(msg);
    }
    SumcheckProof { messages }
}
```
This requirement to pass `&mut sponge` everywhere leads to severe borrow-checker conflicts if the sponge needs to be accessed concurrently, or if the prover needs to hold references to the sponge across asynchronous boundaries (which is common in distributed zero-knowledge provers). 

### Conclusion

This project successfully proves that Haskell is a vastly superior language for *defining* and *auditing* cryptography. While Rust will continue to dominate execution for its raw speed and memory efficiency, a Haskell reference implementation provides the exact mathematical clarity needed to ensure that the underlying protocols are sound, complete, and bug-free before a single line of systems code is written. By comparing our `mlePartialSumProduct` and `driveProverFS` to their Arkworks equivalents, it is undeniable that Haskell bridges the gap between academic cryptography papers and executable code perfectly.
