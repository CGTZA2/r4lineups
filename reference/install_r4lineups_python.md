# Install Python Dependencies for r4lineups

Installs the required Python packages using pip. This function should be
run once after installing the r4lineups package.

## Usage

``` r
install_r4lineups_python(
  method = "auto",
  envname = NULL,
  pip_options = NULL,
  gpu = FALSE
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

## Details

This function installs:

- deepface - Main face analysis library

- opencv-python - Image processing

- numpy - Numerical computing

- tf-keras - TensorFlow/Keras backend

- mediapipe - For 468-point face mesh (optional but recommended)

- retinaface - Best face detector

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic installation
install_r4lineups_python()

# Install to a virtual environment
install_r4lineups_python(envname = "r4lineups-face-env")

# Install with GPU support
install_r4lineups_python(gpu = TRUE)
} # }
```
