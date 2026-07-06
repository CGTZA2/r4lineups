# Extract Embeddings for Multiple Images

Efficiently extracts face embeddings from multiple images.

## Usage

``` r
batch_embeddings(
  img_paths,
  model = "ArcFace",
  detector = "retinaface",
  align = TRUE,
  enforce_detection = TRUE,
  progress = TRUE
)
```

## Arguments

- img_paths:

  Character vector of paths to image files.

- model:

  Character. The face recognition model to use. Default is "ArcFace".

- detector:

  Character. Face detection backend. Default is "retinaface".

- align:

  Logical. Whether to align faces. Default is TRUE.

- enforce_detection:

  Logical. Error if face not detected. Default is TRUE.

- progress:

  Logical. Show progress bar. Default is TRUE.

## Value

A tibble with columns:

- img_path - Path to the image

- embedding - List column containing embedding vectors

- facial_area - List column with face bounding box

- face_confidence - Detection confidence

- success - Logical indicating if extraction succeeded

- error - Error message if extraction failed

## Examples

``` r
if (FALSE) { # \dontrun{
# Get embeddings for all images in a folder
img_files <- list.files("faces/", pattern = "\\.jpg$", full.names = TRUE)
embeddings <- batch_embeddings(img_files)

# Access individual embeddings
embeddings$embedding[[1]]
} # }
```
