# Compute Shannon Entropy

Helper function to compute Shannon entropy for a probability value.
Entropy measures uncertainty about guilt/innocence.

## Usage

``` r
entropy(p, base = 2)
```

## Arguments

- p:

  Probability value (between 0 and 1)

- base:

  Logarithm base (default = 2 for bits)

## Value

Shannon entropy in bits (if base = 2)

## Details

Entropy is computed as: H(p) = -\[p\*log(p) + (1-p)\*log(1-p)\] When p
is 0 or 1 (certainty), entropy is 0. Maximum entropy (1 bit) occurs at p
= 0.5 (maximum uncertainty).
