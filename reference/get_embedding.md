# Extract Face Embedding from an Image

Extracts a deep learning embedding vector from a face image using a
specified model. The embedding can be used to compute face similarity.

## Usage

``` r
get_embedding(
  img_path,
  model = "ArcFace",
  detector = "retinaface",
  align = TRUE,
  enforce_detection = TRUE,
  expand_percentage = 0
)
```

## Arguments

- img_path:

  Path to the image file containing a face.

- model:

  Character. The face recognition model to use. Default is "ArcFace"
  (recommended). See
  [`available_models`](https://cgtza2.github.io/r4lineups/reference/available_models.md).

- detector:

  Character. Face detection backend. Default is "retinaface" (most
  accurate). See
  [`available_detectors`](https://cgtza2.github.io/r4lineups/reference/available_detectors.md).

- align:

  Logical. Whether to align the face before embedding. Default is TRUE
  (recommended).

- enforce_detection:

  Logical. If TRUE, raises an error if no face is detected. If FALSE,
  attempts to use the entire image. Default is TRUE.

- expand_percentage:

  Numeric. Percentage to expand the detected face region. Default is 0.

## Value

A named list containing:

- embedding - Numeric vector of face embeddings

- facial_area - List with x, y, w, h of detected face region

- face_confidence - Detection confidence score

## Details

The embedding dimension depends on the model:

- ArcFace: 512 dimensions

- Facenet512: 512 dimensions

- Facenet: 128 dimensions

- VGG-Face: 4096 dimensions

For best results in research applications, use:

- model = "ArcFace" (best accuracy)

- detector = "retinaface" (best detection)

- align = TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
# Get embedding for a single face
emb <- get_embedding("path/to/face.jpg")

# Access the embedding vector
emb$embedding

# Use a different model
emb_facenet <- get_embedding("path/to/face.jpg", model = "Facenet512")
} # }
```
