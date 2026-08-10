# Compute Face Similarity Between Two Images

Computes the similarity between two face images using deep learning
embeddings. This is the core function for comparing a target face to a
foil face.

## Usage

``` r
face_similarity(
  img1_path,
  img2_path,
  model = "ArcFace",
  metric = "cosine",
  detector = "retinaface",
  return_embeddings = FALSE
)
```

## Arguments

- img1_path:

  Path to the first (target) image.

- img2_path:

  Path to the second (foil) image.

- model:

  Character. Face recognition model. Default is "ArcFace".

- metric:

  Character. Distance metric: "cosine" (recommended), "euclidean", or
  "euclidean_l2".

- detector:

  Character. Face detection backend. Default is "retinaface".

- return_embeddings:

  Logical. If TRUE, also returns the embedding vectors.

## Value

A list containing:

- distance - The computed distance (lower = more similar)

- similarity - Similarity score in \[-1,1\] for cosine metric

- verified - Logical, TRUE if distance is below the model's threshold

- threshold - The verification threshold for this model/metric

- model - Model used

- metric - Metric used

- embeddings - (if return_embeddings=TRUE) List of both embeddings

## Details

This function wraps DeepFace's verify function but is designed for
similarity measurement rather than identity verification. In lineup
research, we typically want to measure how similar foils are to the
target, not whether they are the same person.

The 'verified' field indicates whether the faces would be considered
"the same person" by the model, which in lineup research context means
the foil is too similar to the target (potential lineup bias).

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare target face to a foil
result <- face_similarity("target.jpg", "foil1.jpg")

# Check similarity score
result$similarity  # 0.85 would mean very similar

# Check if they'd be identified as same person (too similar for fair lineup)
result$verified  # TRUE would indicate problematic similarity
} # }
```
