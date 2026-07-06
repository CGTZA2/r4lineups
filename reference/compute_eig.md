# Compute Expected Information Gain (EIG)

Computes Expected Information Gain for lineup identification procedures
following Starns et al. (2023). EIG quantifies the evidentiary value of
witness responses using information theory.

## Usage

``` r
compute_eig(eig_data, prior_guilt = 0.5, confidence_bins = NULL)
```

## Arguments

- eig_data:

  List output from make_eig_data(), or a dataframe with columns:
  target_present, identification, confidence

- prior_guilt:

  Numeric. Prior probability that suspect is guilty (default = 0.5)

- confidence_bins:

  Numeric vector of bin edges (only used if eig_data is a dataframe)

## Value

A list of class "lineup_eig" containing:

- eig: Expected Information Gain in bits

- response_data: Dataframe with information gain for each response
  category

- prior_guilt: Prior probability used

- prior_entropy: Entropy of prior (baseline uncertainty)

- n_guilty: Number of target-present lineups

- n_innocent: Number of target-absent lineups

## Details

EIG measures how much information (in bits) a witness response provides
about guilt vs. innocence. It is computed as:

\$\$EIG = \sum_x p(x) \times \[H(prior) - H(p(guilty\|x))\]\$\$

where:

- x = response category (e.g., "suspect_high_confidence")

- p(x) = probability of that response

- H() = Shannon entropy

- p(guilty\|x) = posterior probability of guilt given response x

Higher EIG values indicate more diagnostic procedures. EIG = 0 means no
information gain (responses don't distinguish guilty from innocent). EIG
= 1 means perfect information (complete resolution of uncertainty).

## References

Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical
conclusions via the data they should have produced: A priori comparison
of eyewitness identification decision processes using quantitative
predictions of the expected information gain. *Psychological Review*.

## Examples

``` r
# Compute EIG with binned confidence
data(lineup_example)
eig_result <- compute_eig(lineup_example,
                          prior_guilt = 0.5,
                          confidence_bins = c(0, 60, 80, 100))
print(eig_result)
#> 
#> === Expected Information Gain (EIG) Analysis ===
#> 
#> EIG: 0.2836 bits
#> Prior probability of guilt: 0.5 
#> Prior entropy: 1 bits
#> Information efficiency: 28.4 %
#> 
#> Sample sizes:
#>   Target-present (guilty): 100 
#>   Target-absent (innocent): 100 
#> 
#> Confidence bins: 0, 60, 80, 100 
#> 
#> Top response categories by information gain:
#> (Showing up to 10 of 6 total)
#> 
#> # A tibble: 6 × 6
#>   response         identification confidence_level p_response posterior_guilty
#>   <chr>            <chr>          <chr>            <chr>      <chr>           
#> 1 suspect_(80,100] suspect        (80,100]         0.190      1.000           
#> 2 filler_(60,80]   filler         (60,80]          0.040      0.125           
#> 3 reject_[0,60]    reject         [0,60]           0.415      0.265           
#> 4 suspect_(60,80]  suspect        (60,80]          0.135      0.630           
#> 5 filler_[0,60]    filler         [0,60]           0.170      0.500           
#> 6 suspect_[0,60]   suspect        [0,60]           0.050      0.500           
#> # ℹ 1 more variable: information_gain <chr>
#> 
#> Full response data available in $response_data
```
