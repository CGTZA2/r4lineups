# Apply Decision Rule to Lineup Strengths

Internal helper function that computes decision variable based on
different decision strategies.

## Usage

``` r
.apply_decision_rule(strengths, rule, lineup_size, d_prime)
```

## Arguments

- strengths:

  Numeric vector of memory strengths for all lineup members

- rule:

  Character. Decision rule to apply

- lineup_size:

  Integer. Number of lineup members

- d_prime:

  Numeric. Used for some rule calculations

## Value

List with decision_value and chosen_position
