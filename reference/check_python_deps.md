# Check Python Dependencies

Verifies that all required Python packages are installed and reports
their versions.

## Usage

``` r
check_python_deps(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. If TRUE, prints detailed information.

## Value

A tibble with package names, installed status, and versions. Its
`"face_mesh"` attribute says whether
[`get_face_landmarks()`](https://cgtza2.github.io/r4lineups/reference/get_face_landmarks.md)
can run: `"solutions"`, `"tasks"` or `"unavailable"`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_python_deps()
} # }
```
