# Install the Audited pyWitness Engine

Explicitly creates or reuses an isolated Python virtual environment and
installs the audited pyWitness Git revision used by
[`fit_lineup_models`](https://cgtza2.github.io/r4lineups/reference/fit_lineup_models.md).
This function is never called when the package is loaded or checked.

## Usage

``` r
install_pywitness(
  envname = .pywitness_default_env,
  python = NULL,
  pip_options = NULL,
  force = FALSE
)
```

## Arguments

- envname:

  Name or path of the virtual environment.

- python:

  Optional path to the Python executable used to create a new virtual
  environment.

- pip_options:

  Optional character vector passed to pip.

- force:

  Logical. Reinstall the pinned revision if already present.

## Value

Invisibly returns the environment name.

## Details

The installation is pinned to Git revision
`e726dcfc09423d0e0ff7f46c8e3a711040293eba`. pyWitness is GPL-3 software
maintained separately from r4lineups; no pyWitness source or
documentation is bundled in this package. The immutable commit archive
is used so installation does not require a local Git executable.

## References

Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
pyWitness 1.0: A Python eyewitness identification analysis toolkit.
*Behavior Research Methods, 56*, 1533–1550.

## Examples

``` r
if (FALSE) { # \dontrun{
install_pywitness()
check_pywitness_deps(initialize = TRUE)
} # }
```
