# Get Key Facial Landmarks (5-Point)

Extracts the 5 key facial landmarks commonly used for face alignment:
left eye, right eye, nose tip, left mouth corner, right mouth corner.

## Usage

``` r
get_key_landmarks(img_path, detector = "retinaface")
```

## Arguments

- img_path:

  Path to the image file.

- detector:

  Detector backend. "retinaface" provides these landmarks directly.

## Value

A list with named landmark coordinates:

- left_eye - (x, y) coordinates

- right_eye - (x, y) coordinates

- nose - (x, y) coordinates

- mouth_left - (x, y) coordinates

- mouth_right - (x, y) coordinates

## Details

These 5 landmarks are sufficient for basic face alignment and are
commonly used in face recognition pipelines. For more detailed landmarks
(468 points), use
[`get_face_landmarks`](https://cgtza2.github.io/r4lineups/reference/get_face_landmarks.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get key landmarks
lm <- get_key_landmarks("face.jpg")

# Calculate inter-pupillary distance
ipd <- sqrt(sum((lm$left_eye - lm$right_eye)^2))
} # }
```
