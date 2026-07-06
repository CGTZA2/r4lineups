# Detect Faces in an Image

Detects all faces in an image and returns their bounding boxes and
facial landmarks.

## Usage

``` r
detect_faces(img_path, detector = "retinaface")
```

## Arguments

- img_path:

  Path to the image file.

- detector:

  Character. Face detection backend to use. Options: "retinaface"
  (default, most accurate), "mtcnn", "mediapipe", "opencv", "ssd",
  "dlib", "yolov8", "centerface".

## Value

A list where each element represents a detected face with:

- facial_area - List with x, y, w, h coordinates

- landmarks - List with eye, nose, mouth positions (if available)

- confidence - Detection confidence score

Returns an empty list if no faces are detected.

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect faces in an image
faces <- detect_faces("group_photo.jpg")

# Number of faces found
length(faces)

# Get bounding box of first face
faces[[1]]$facial_area
} # }
```
