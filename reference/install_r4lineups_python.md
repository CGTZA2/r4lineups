# Install Python Dependencies for r4lineups

Installs the required Python packages using pip. This function should be
run once after installing the r4lineups package.

## Usage

``` r
install_r4lineups_python(
  method = "auto",
  envname = NULL,
  pip_options = NULL,
  gpu = FALSE,
  python_version = NULL
)
```

## Arguments

- method:

  Installation method. Default is "auto" which uses pip.

- envname:

  Name of virtual environment to use (optional). If NULL, installs to
  the system Python or currently active environment.

- pip_options:

  Additional options to pass to pip (e.g., "–user").

- gpu:

  Logical. If TRUE, installs tensorflow with GPU support. Default is
  FALSE.

- python_version:

  Python version to build the virtual environment with, e.g. `"3.11"`.
  Defaults to `NULL`, which leaves the choice to reticulate but still
  checks the result against the supported range.

## Value

`TRUE`, invisibly.

## Details

This function installs:

- deepface - Main face analysis library

- opencv-python - Image processing

- numpy - Numerical computing

- tf-keras - TensorFlow/Keras backend

- mediapipe - For 478-point face mesh (optional but recommended)

- retinaface - Best face detector

**Python version.** TensorFlow publishes wheels for a limited range of
Python versions (currently 3.9 to 3.12). On a machine whose default
interpreter is newer than that, pip finds no TensorFlow distribution at
all and the install fails partway through with
`No matching distribution found for tensorflow`, after downloading the
other packages. This function therefore checks the interpreter before
installing anything, and stops with an explanatory message if it is
outside the supported range. Pass `python_version` to select a suitable
interpreter, which requires `envname` so that a virtual environment can
be built from it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic installation
install_r4lineups_python()

# Install to a virtual environment
install_r4lineups_python(envname = "r4lineups-face-env")

# Build the environment with a TensorFlow-supported interpreter
install_r4lineups_python(envname = "r4lineups-face-env",
                         python_version = "3.11")

# Install with GPU support
install_r4lineups_python(gpu = TRUE)
} # }
```
