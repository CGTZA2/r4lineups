# Check Availability of the pyWitness Engine

Reports whether Python and pyWitness are available without initializing
Python by default.

## Usage

``` r
check_pywitness_deps(
  envname = .pywitness_default_env,
  initialize = FALSE,
  verbose = TRUE
)
```

## Arguments

- envname:

  Name or path of the intended virtual environment.

- initialize:

  Logical. If `TRUE`, select `envname`, initialize Python, and import
  pyWitness. The default performs no initialization.

- verbose:

  Logical. Print a concise status report.

## Value

A one-row tibble with environment and engine status, including whether
the imported version encodes the audited revision when
`initialize = TRUE`.

## Examples

``` r
check_pywitness_deps()
#> pyWitness environment: r4lineups-pywitness 
#> Environment exists: FALSE 
#> Python initialized: FALSE 
#> pyWitness available: FALSE 
#> Audited revision: e726dcfc09423d0e0ff7f46c8e3a711040293eba 
#> No Python process was initialized. Use initialize = TRUE to import the isolated engine.
if (FALSE) { # \dontrun{
check_pywitness_deps(initialize = TRUE)
} # }
```
