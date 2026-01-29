#' Install Python Dependencies for faceR
#'
#' Installs the required Python packages using pip. This function should be run
#' once after installing the r4lineups package.
#'
#' @param method Installation method. Default is "auto" which uses pip.
#' @param envname Name of virtual environment to use (optional). If NULL,
#'   installs to the system Python or currently active environment.
#' @param pip_options Additional options to pass to pip (e.g., "--user").
#' @param gpu Logical. If TRUE, installs tensorflow with GPU support.
#'   Default is FALSE.
#'
#' @details
#' This function installs:
#' \itemize{
#'   \item deepface - Main face analysis library
#'   \item opencv-python - Image processing
#'   \item numpy - Numerical computing
#'   \item tf-keras - TensorFlow/Keras backend
#'   \item mediapipe - For 468-point face mesh (optional but recommended)
#'   \item retinaface - Best face detector
#' }
#'
#' @examples
#' \dontrun{
#' # Basic installation
#' install_r4lineups_python()
#'
#' # Install to a virtual environment
#' install_r4lineups_python(envname = "faceR-env")
#'
#' # Install with GPU support
#' install_r4lineups_python(gpu = TRUE)
#' }
#'
#' @export
install_r4lineups_python <- function(method = "auto",
                                  envname = NULL,
                                  pip_options = NULL,
                                  gpu = FALSE) {

  packages <- c(
    "deepface",
    "opencv-python",
    "numpy",
    "tf-keras",
    "mediapipe",
    "retina-face"
  )

  # Add tensorflow with appropriate variant
  if (gpu) {
    packages <- c(packages, "tensorflow[and-cuda]")
    message("Installing with GPU support...")
  } else {
    packages <- c(packages, "tensorflow")
  }

  message("Installing Python dependencies for r4lineups...")
  message("Packages: ", paste(packages, collapse = ", "))

  if (!is.null(envname)) {
    # Create virtual environment if it doesn't exist
    if (!reticulate::virtualenv_exists(envname)) {
      message("Creating virtual environment: ", envname)
      reticulate::virtualenv_create(envname)
    }
    reticulate::use_virtualenv(envname, required = TRUE)
  }

  # Install packages
  reticulate::py_install(
    packages = packages,
    pip = TRUE,
    pip_options = pip_options
  )

  message("\nInstallation complete!")
  message("Run check_python_deps() to verify the installation.")

  invisible(TRUE)
}


#' Check Python Dependencies
#'
#' Verifies that all required Python packages are installed and reports
#' their versions.
#'
#' @param verbose Logical. If TRUE, prints detailed information.
#'
#' @return A tibble with package names, installed status, and versions.
#'
#' @examples
#' \dontrun{
#' check_python_deps()
#' }
#'
#' @export
check_python_deps <- function(verbose = TRUE) {

  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Python is not available. Please install Python first.", call. = FALSE)
  }

  packages <- c(
    "deepface",
    "cv2",
    "numpy",
    "tensorflow",
    "mediapipe",
    "retinaface"
  )

  # Display names (cv2 is imported as cv2 but installed as opencv-python)
  display_names <- c(
    "deepface",
    "opencv-python (cv2)",
    "numpy",
    "tensorflow",
    "mediapipe",
    "retinaface"
  )

  results <- tibble::tibble(
    package = display_names,
    installed = logical(length(packages)),
    version = character(length(packages)),
    required = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  for (i in seq_along(packages)) {
    pkg <- packages[i]
    results$installed[i] <- reticulate::py_module_available(pkg)

    if (results$installed[i]) {
      tryCatch({
        mod <- reticulate::import(pkg)
        results$version[i] <- if (!is.null(mod$`__version__`)) {
          as.character(mod$`__version__`)
        } else {
          "unknown"
        }
      }, error = function(e) {
        results$version[i] <- "error"
      })
    } else {
      results$version[i] <- NA_character_
    }
  }

  if (verbose) {
    cat("\n=== r4lineups Python Dependencies ===\n\n")
    cat(sprintf("Python version: %s\n", reticulate::py_config()$version))
    cat(sprintf("Python path: %s\n\n", reticulate::py_config()$python))

    for (i in seq_len(nrow(results))) {
      status <- if (results$installed[i]) {
        paste0("\u2713 ", results$version[i])
      } else if (results$required[i]) {
        "\u2717 MISSING (required)"
      } else {
        "\u2717 not installed (optional)"
      }
      cat(sprintf("%-25s %s\n", results$package[i], status))
    }

    # Summary
    missing_required <- sum(!results$installed & results$required)
    if (missing_required > 0) {
      cat("\n\u26A0 ", missing_required, " required package(s) missing.\n")
      cat("Run install_r4lineups_python() to install them.\n")
    } else {
      cat("\n\u2713 All required packages installed!\n")
    }
  }

  invisible(results)
}


#' List Available Face Recognition Models
#'
#' Returns a character vector of available face recognition models
#' that can be used for embedding extraction.
#'
#' @return Character vector of model names.
#'
#' @details
#' Available models with their embedding dimensions:
#' \itemize{
#'   \item VGG-Face (4096-d) - Legacy model
#'   \item Facenet (128-d) - Google's FaceNet
#'   \item Facenet512 (512-d) - FaceNet with larger embeddings
#'   \item OpenFace (128-d) - Open source
#'   \item DeepFace (4096-d) - Facebook's model
#'   \item DeepID (160-d) - Compact model
#'   \item ArcFace (512-d) - State-of-the-art, recommended
#'   \item Dlib (128-d) - Lightweight
#'   \item SFace (128-d) - Lightweight
#'   \item GhostFaceNet (512-d) - Efficient
#' }
#'
#' @examples
#' available_models()
#'
#' @export
available_models <- function() {
  c(
    "VGG-Face",
    "Facenet",
    "Facenet512",
    "OpenFace",
    "DeepFace",
    "DeepID",
    "ArcFace",
    "Dlib",
    "SFace",
    "GhostFaceNet"
  )
}


#' List Available Face Detectors
#'
#' Returns a character vector of available face detection backends.
#'
#' @return Character vector of detector names.
#'
#' @details
#' Available detectors (in rough order of accuracy/speed tradeoff):
#' \itemize{
#'   \item retinaface - Most accurate, recommended for research
#'   \item mtcnn - Good accuracy, moderate speed
#'   \item mediapipe - Fast, good for real-time
#'   \item opencv - Fastest, less accurate
#'   \item ssd - Fast CNN-based
#'   \item dlib - Classic HOG-based
#'   \item yolov8 - YOLO-based detection
#'   \item centerface - Anchor-free detection
#'   \item skip - Skip detection (use full image)
#' }
#'
#' @examples
#' available_detectors()
#'
#' @export
available_detectors <- function() {
  c(
    "retinaface",
    "mtcnn",
    "mediapipe",
    "opencv",
    "ssd",
    "dlib",
    "yolov8",
    "centerface",
    "skip"
  )
}
