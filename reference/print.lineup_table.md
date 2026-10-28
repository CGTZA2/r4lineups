# Print Method for lineup_table Objects

Print Method for lineup_table Objects

## Usage

``` r
# S3 method for class 'lineup_table'
print(x, ...)
```

## Arguments

- x:

  A `lineup_table` object.

- ...:

  Additional arguments (ignored).

## Value

`x`, invisibly.

## Examples

``` r
print(lineup_table(rep(c(5, 3, 2, 1, 8, 6, 7), each = 4), k = 8))
#> Lineup choices (k = 8, N = 28)
#> 
#>  pos: 1  2  3  4  5  6  7  8
#>  cnt: 4  4  4  0  4  4  4  4
#> 
#>  Note: 1 null foil (position 4) received no choices.
#>  Unchosen members reduce effective size; see ?esize_m.
```
