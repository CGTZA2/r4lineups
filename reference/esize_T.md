# Tredoux Effective Size (Tredoux, 1998)

Function for generating Tredoux's transformed index-of-diversity
effective size.

## Usage

``` r
esize_T(lineup_table)
```

## Arguments

- lineup_table:

  A table of lineup choices

## Details

Tredoux (1998) proposed using Agresti and Agresti's (1978) index of
diversity for lineup choices, \$\$I = 1 - \frac{\sum_i o_i^2}{N^2},\$\$
where \\o_i\\ is the observed count for lineup member \\i\\ and \\N\\ is
the total number of choices. The index is then transformed back to an
effective-size scale: \$\$E' = \frac{1}{1-I} = \frac{N^2}{\sum_i o_i^2}
= \frac{1}{\sum_i p_i^2}.\$\$ This formula is algebraically equivalent
to a reciprocal concentration form.

## References

Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
identification lineups. *Law and Human Behavior, 5*(4), 299-309.

Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
qualitative variation. *Sociological Methodology, 9*, 204-237.

Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
& M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory
for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.

Tredoux, C. G. (1998). Statistical inference on measures of lineup
fairness. *Law and Human Behavior, 22(2)*, 217-237.

Tredoux, C. (1999). Statistical considerations when determining measures
of lineup size and lineup bias. *Applied Cognitive Psychology, 13*,
S9-S26.

Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
empirically assessing the fairness of a lineup. *Law and Human Behavior,
3*(4), 285-293.

## Examples

``` r
#Data:
lineup_vec <- round(runif(100, 1, 6))
lineup_table <- table(lineup_vec)

#Call:
e <- esize_T(lineup_table)
```
