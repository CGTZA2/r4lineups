# Convert Cosine Distance to Similarity Score

Converts cosine distance to cosine similarity in \[-1, 1\].

## Usage

``` r
cosine_to_similarity(distance)
```

## Arguments

- distance:

  Numeric. Cosine distance value(s).

## Value

Numeric cosine similarity score(s), where 1 means the same direction, 0
means orthogonal, and -1 means opposite directions.

## Examples

``` r
# Distance of 0 -> similarity of 1
cosine_to_similarity(0)  # Returns 1
#> [1] 1

# Distance of 0.5 -> similarity of 0.5
cosine_to_similarity(0.5)  # Returns 0.5
#> [1] 0.5
```
