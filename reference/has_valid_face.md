# Check if Image Contains a Valid Face

Quick check to determine if an image contains a detectable face that can
be used for embedding extraction.

## Usage

``` r
has_valid_face(img_path, detector = "retinaface", min_confidence = 0.5)
```

## Arguments

- img_path:

  Path to the image file.

- detector:

  Detector to use.

- min_confidence:

  Minimum detection confidence (0-1).

## Value

Logical. TRUE if a valid face is detected.

## Examples

``` r
if (FALSE) { # \dontrun{
if (has_valid_face("image.jpg")) {
  emb <- get_embedding("image.jpg")
}
} # }
```
