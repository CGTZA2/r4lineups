# Plot Embedding Space

Creates a 2D visualization of face embeddings using dimensionality
reduction, showing how faces cluster in the embedding space.

## Usage

``` r
plot_embedding_space(
  embeddings_df,
  method = c("umap", "pca", "tsne"),
  labels = NULL,
  highlight = NULL,
  n_neighbors = 15,
  perplexity = 30
)
```

## Arguments

- embeddings_df:

  A tibble from
  [`batch_embeddings`](https://cgtza2.github.io/r4lineups/reference/batch_embeddings.md)
  with an 'embedding' list column.

- method:

  Dimensionality reduction method: "umap" (default), "pca", or "tsne".

- labels:

  Optional character vector of labels for each point.

- highlight:

  Optional indices of points to highlight (e.g., target position).

- n_neighbors:

  For UMAP, number of neighbors. Default is 15.

- perplexity:

  For t-SNE, perplexity parameter. Default is 30.

## Value

A ggplot2 object.

## Details

This visualization helps understand:

- How faces cluster by identity

- Which foils are closest to the target in embedding space

- Overall structure of similarity in a lineup

## Examples

``` r
if (FALSE) { # \dontrun{
# Get embeddings for all lineup members
all_faces <- c("target.jpg", paste0("foil", 1:5, ".jpg"))
embs <- batch_embeddings(all_faces)

# Plot with target highlighted
plot_embedding_space(embs, labels = c("Target", paste0("F", 1:5)), highlight = 1)
} # }
```
