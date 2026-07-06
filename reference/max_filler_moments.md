# Max Filler Distribution Moments

Numeric moments of the max filler distribution using integration.

## Usage

``` r
max_filler_moments(
  lineup_size,
  moments = c("mean", "var", "skewness"),
  rel.tol = 1e-08,
  subdivisions = 2000
)
```

## Arguments

- lineup_size:

  Lineup size (suspect + fillers), must be \>= 2.

- moments:

  Character vector specifying moments to return. Supported: "mean",
  "var", "skewness".

- rel.tol:

  Relative tolerance passed to integrate().

- subdivisions:

  Maximum subdivisions for integrate().

## Value

Named list of moments.

## Examples

``` r
max_filler_moments(6)
#> $mean
#> [1] 1.162964
#> 
#> $var
#> [1] 0.4475341
#> 
#> $skewness
#> [1] 0.3025709
#> 
```
