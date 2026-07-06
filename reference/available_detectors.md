# List Available Face Detectors

Returns a character vector of available face detection backends.

## Usage

``` r
available_detectors()
```

## Value

Character vector of detector names.

## Details

Available detectors (in rough order of accuracy/speed tradeoff):

- retinaface - Most accurate, recommended for research

- mtcnn - Good accuracy, moderate speed

- mediapipe - Fast, good for real-time

- opencv - Fastest, less accurate

- ssd - Fast CNN-based

- dlib - Classic HOG-based

- yolov8 - YOLO-based detection

- centerface - Anchor-free detection

- skip - Skip detection (use full image)

## Examples

``` r
available_detectors()
#> [1] "retinaface" "mtcnn"      "mediapipe"  "opencv"     "ssd"       
#> [6] "dlib"       "yolov8"     "centerface" "skip"      
```
