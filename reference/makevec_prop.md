# Helper functions

Several helper functions that compute bootstrap confidence limits from
proportions.

## Usage

``` r
makevec_prop(prop, n)
```

## Arguments

- prop:

  A single finite proportion between 0 and 1.

- n:

  A positive whole-number sample size. `n * prop` must be a whole
  number.

## Examples

``` r
# Expand a choice proportion of 0.25 among 20 mock witnesses into a
# binary choice vector
makevec_prop(0.25, 20)
#>  [1] 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```
