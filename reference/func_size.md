# Functional Size

This function computes functional size (N/D), where N = number of mock
witnesses, & D = number of mock witnesses choosing the target

## Usage

``` r
func_size(lineup_vec, target_pos)
```

## Arguments

- lineup_vec:

  A numeric vector of lineup choices

- target_pos:

  A scalar, representing target position in lineup. Must be declared by
  user

## Value

Functional size of lineup

## Details

An estimation of the number of plausible lineup members in a given
lineup.

## References

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22*(2), 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology, 13*,
S9-S26.

Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
target_pos <- 3

#Call:
x <- func_size(lineup_vec, target_pos)
x <- func_size(lineup_vec, 3)
```
