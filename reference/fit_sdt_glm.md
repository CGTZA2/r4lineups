# Fit an SDT GLM for Old/New Recognition

Fits a single-level GLM (binomial link) to estimate SDT metrics from
old/new recognition responses. With centered item coding, the
coefficient on item status corresponds to d' (probit) or ln(OR) (logit).

## Usage

``` r
fit_sdt_glm(
  data,
  is_old,
  said_old,
  covariates = NULL,
  link = "probit",
  center_isold = TRUE,
  interactions = FALSE
)
```

## Arguments

- data:

  Data frame containing trial-level responses.

- is_old:

  Column name indicating whether the item is old (1) or new (0).

- said_old:

  Column name indicating whether the participant said "old" (1) or "new"
  (0).

- covariates:

  Optional character vector of covariate column names.

- link:

  Link function for the binomial model ("probit" or "logit").

- center_isold:

  Logical. If TRUE, recodes is_old to -0.5 / +0.5.

- interactions:

  Logical. If TRUE, include is_old interactions with covariates.

## Value

A `glm` object with SDT attributes.

## Examples

``` r
set.seed(123)
n <- 200
is_old <- rep(c(0, 1), each = n / 2)
# d' of about 1: higher hit rate for old items
said_old <- rbinom(n, 1, ifelse(is_old == 1, 0.7, 0.3))
df <- data.frame(is_old = is_old, said_old = said_old)
fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old")
extract_sdt_metrics(fit)
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.19673
#> 
#> $criterion
#> [1] -0.04498034
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
#> 
```
