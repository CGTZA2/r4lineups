# Compute Distance Between Two Embeddings

Calculates the distance or similarity between two face embedding
vectors.

## Usage

``` r
embedding_distance(
  emb1,
  emb2,
  metric = c("cosine", "euclidean", "euclidean_l2")
)
```

## Arguments

- emb1:

  Numeric vector. First embedding.

- emb2:

  Numeric vector. Second embedding.

- metric:

  Character. Distance metric to use:

  - "cosine" - Cosine distance (1 - cosine similarity). Recommended for
    ArcFace.

  - "euclidean" - Euclidean (L2) distance.

  - "euclidean_l2" - Euclidean distance on L2-normalized vectors.

## Value

Numeric distance value. Lower values indicate more similar faces.

## Details

For ArcFace and other angular-margin trained models, cosine distance is
recommended as it aligns with the training objective.

Cosine distance ranges from 0 (identical) to 2 (opposite). Typical
thresholds for same-person verification:

- ArcFace + cosine: ~0.68

- Facenet512 + cosine: ~0.30

## Examples

``` r
if (FALSE) { # \dontrun{
emb1 <- get_embedding("face1.jpg")$embedding
emb2 <- get_embedding("face2.jpg")$embedding

# Cosine distance (recommended)
embedding_distance(emb1, emb2, metric = "cosine")

# Euclidean distance
embedding_distance(emb1, emb2, metric = "euclidean")
} # }
```
