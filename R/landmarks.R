#' Detect Faces in an Image
#'
#' Detects all faces in an image and returns their bounding boxes
#' and facial landmarks.
#'
#' @param img_path Path to the image file.
#' @param detector Character. Face detection backend to use.
#'   Options: "retinaface" (default, most accurate), "mtcnn", 
#'   "mediapipe", "opencv", "ssd", "dlib", "yolov8", "centerface".
#'
#' @return A list where each element represents a detected face with:
#' \itemize{
#'   \item facial_area - List with x, y, w, h coordinates
#'   \item landmarks - List with eye, nose, mouth positions (if available)
#'   \item confidence - Detection confidence score
#' }
#'
#' Returns an empty list if no faces are detected.
#'
#' @examples
#' \dontrun{
#' # Detect faces in an image
#' faces <- detect_faces("group_photo.jpg")
#' 
#' # Number of faces found
#' length(faces)
#' 
#' # Get bounding box of first face
#' faces[[1]]$facial_area
#' }
#'
#' @export
detect_faces <- function(img_path, detector = "retinaface") {
  
  if (!file.exists(img_path)) {
    stop("Image file not found: ", img_path, call. = FALSE)
  }
  
  detector <- match.arg(detector, available_detectors())
  
  deepface <- get_deepface()
  
  tryCatch({
    # Use extract_faces to get detection info
    result <- deepface$extract_faces(
      img_path = normalizePath(img_path),
      detector_backend = detector,
      enforce_detection = FALSE,
      align = FALSE
    )
    
    faces <- lapply(result, function(face) {
      list(
        facial_area = face$facial_area,
        confidence = face$confidence
      )
    })
    
    faces
    
  }, error = function(e) {
    warning("Error detecting faces: ", e$message)
    list()
  })
}


#' Get 468-Point Face Mesh Landmarks
#'
#' Extracts dense facial landmarks using MediaPipe Face Mesh,
#' which provides 468 3D landmark points covering the entire face.
#'
#' @param img_path Path to the image file.
#' @param return_3d Logical. If TRUE, returns 3D coordinates (x, y, z).
#'   If FALSE, returns only 2D coordinates (x, y). Default is TRUE.
#'
#' @return A list containing:
#' \itemize{
#'   \item landmarks - Matrix of landmark coordinates (468 x 2 or 468 x 3)
#'   \item face_detected - Logical indicating if a face was found
#'   \item image_width - Width of the input image
#'   \item image_height - Height of the input image
#' }
#'
#' @details
#' MediaPipe Face Mesh provides detailed landmarks covering:
#' \itemize{
#'   \item Face oval/contour
#'   \item Left and right eyebrows
#'   \item Left and right eyes (including iris)
#'   \item Nose
#'   \item Lips (inner and outer contours)
#'   \item Face interior regions (cheeks, forehead)
#' }
#'
#' The x and y coordinates are normalized to [0, 1] relative to image
#' dimensions. The z coordinate represents depth relative to the face center.
#'
#' @note Requires MediaPipe to be installed (installed by default with
#'   install_faceR_python()).
#'
#' @examples
#' \dontrun{
#' # Get face mesh landmarks
#' mesh <- get_face_landmarks("face.jpg")
#' 
#' # Plot the landmarks
#' plot(mesh$landmarks[, 1], mesh$landmarks[, 2], pch = ".")
#' }
#'
#' @export
get_face_landmarks <- function(img_path, return_3d = TRUE) {
  
  if (!file.exists(img_path)) {
    stop("Image file not found: ", img_path, call. = FALSE)
  }
  
  mediapipe <- get_mediapipe()
  cv2 <- get_cv2()
  np <- get_numpy()
  
  if (is.null(mediapipe)) {
    stop("MediaPipe is not installed. Run: pip install mediapipe", call. = FALSE)
  }
  
  tryCatch({
    # Read image
    img <- cv2$imread(normalizePath(img_path))
    if (is.null(img)) {
      stop("Could not read image: ", img_path)
    }
    
    # Convert BGR to RGB
    img_rgb <- cv2$cvtColor(img, cv2$COLOR_BGR2RGB)
    
    # Get image dimensions
    img_height <- as.integer(dim(img)[1])
    img_width <- as.integer(dim(img)[2])
    
    # Initialize Face Mesh
    mp_face_mesh <- mediapipe$solutions$face_mesh
    face_mesh <- mp_face_mesh$FaceMesh(
      static_image_mode = TRUE,
      max_num_faces = 1L,
      refine_landmarks = TRUE,  # Include iris landmarks
      min_detection_confidence = 0.5
    )
    
    # Process image
    results <- face_mesh$process(img_rgb)
    
    # Check if face was detected
    if (is.null(results$multi_face_landmarks) || 
        length(results$multi_face_landmarks) == 0) {
      return(list(
        landmarks = NULL,
        face_detected = FALSE,
        image_width = img_width,
        image_height = img_height
      ))
    }
    
    # Extract landmarks from first face
    face_landmarks <- results$multi_face_landmarks[[1]]
    
    n_landmarks <- length(face_landmarks$landmark)
    
    if (return_3d) {
      landmarks <- matrix(0, nrow = n_landmarks, ncol = 3)
      colnames(landmarks) <- c("x", "y", "z")
    } else {
      landmarks <- matrix(0, nrow = n_landmarks, ncol = 2)
      colnames(landmarks) <- c("x", "y")
    }
    
    for (i in seq_len(n_landmarks)) {
      lm <- face_landmarks$landmark[[i - 1]]  # Python 0-indexed
      landmarks[i, 1] <- lm$x
      landmarks[i, 2] <- lm$y
      if (return_3d) {
        landmarks[i, 3] <- lm$z
      }
    }
    
    # Clean up
    face_mesh$close()
    
    list(
      landmarks = landmarks,
      face_detected = TRUE,
      image_width = img_width,
      image_height = img_height,
      n_landmarks = n_landmarks
    )
    
  }, error = function(e) {
    stop("Error extracting landmarks: ", e$message, call. = FALSE)
  })
}


#' Get Key Facial Landmarks (5-Point)
#'
#' Extracts the 5 key facial landmarks commonly used for face alignment:
#' left eye, right eye, nose tip, left mouth corner, right mouth corner.
#'
#' @param img_path Path to the image file.
#' @param detector Detector backend. "retinaface" provides these landmarks
#'   directly.
#'
#' @return A list with named landmark coordinates:
#' \itemize{
#'   \item left_eye - (x, y) coordinates
#'   \item right_eye - (x, y) coordinates
#'   \item nose - (x, y) coordinates
#'   \item mouth_left - (x, y) coordinates
#'   \item mouth_right - (x, y) coordinates
#' }
#'
#' @details
#' These 5 landmarks are sufficient for basic face alignment and are
#' commonly used in face recognition pipelines. For more detailed
#' landmarks (468 points), use \code{\link{get_face_landmarks}}.
#'
#' @examples
#' \dontrun{
#' # Get key landmarks
#' lm <- get_key_landmarks("face.jpg")
#' 
#' # Calculate inter-pupillary distance
#' ipd <- sqrt(sum((lm$left_eye - lm$right_eye)^2))
#' }
#'
#' @export
get_key_landmarks <- function(img_path, detector = "retinaface") {
  
  if (!file.exists(img_path)) {
    stop("Image file not found: ", img_path, call. = FALSE)
  }
  
  if (detector != "retinaface") {
    warning("Only 'retinaface' detector provides 5-point landmarks directly. ",
            "Using retinaface.")
    detector <- "retinaface"
  }
  
  # Try to import retinaface directly for landmarks
  tryCatch({
    retinaface <- reticulate::import("retinaface")
    
    resp <- retinaface$RetinaFace$detect_faces(normalizePath(img_path))
    
    if (length(resp) == 0) {
      return(list(
        face_detected = FALSE,
        landmarks = NULL
      ))
    }
    
    # Get first face
    face <- resp[[1]]
    lm <- face$landmarks
    
    list(
      face_detected = TRUE,
      left_eye = c(x = lm$left_eye[[1]], y = lm$left_eye[[2]]),
      right_eye = c(x = lm$right_eye[[1]], y = lm$right_eye[[2]]),
      nose = c(x = lm$nose[[1]], y = lm$nose[[2]]),
      mouth_left = c(x = lm$mouth_left[[1]], y = lm$mouth_left[[2]]),
      mouth_right = c(x = lm$mouth_right[[1]], y = lm$mouth_right[[2]]),
      facial_area = face$facial_area,
      confidence = face$score
    )
    
  }, error = function(e) {
    # Fallback to mediapipe subset if retinaface fails
    message("RetinaFace not available, falling back to MediaPipe subset")
    
    mesh <- get_face_landmarks(img_path, return_3d = FALSE)
    
    if (!mesh$face_detected) {
      return(list(face_detected = FALSE, landmarks = NULL))
    }
    
    # MediaPipe landmark indices for key points
    # (approximate mapping to 5-point landmarks)
    LEFT_EYE <- 33
    RIGHT_EYE <- 263
    NOSE_TIP <- 1
    MOUTH_LEFT <- 61
    MOUTH_RIGHT <- 291
    
    lm <- mesh$landmarks
    w <- mesh$image_width
    h <- mesh$image_height
    
    list(
      face_detected = TRUE,
      left_eye = c(x = lm[LEFT_EYE, 1] * w, y = lm[LEFT_EYE, 2] * h),
      right_eye = c(x = lm[RIGHT_EYE, 1] * w, y = lm[RIGHT_EYE, 2] * h),
      nose = c(x = lm[NOSE_TIP, 1] * w, y = lm[NOSE_TIP, 2] * h),
      mouth_left = c(x = lm[MOUTH_LEFT, 1] * w, y = lm[MOUTH_LEFT, 2] * h),
      mouth_right = c(x = lm[MOUTH_RIGHT, 1] * w, y = lm[MOUTH_RIGHT, 2] * h)
    )
  })
}


#' Check if Image Contains a Valid Face
#'
#' Quick check to determine if an image contains a detectable face
#' that can be used for embedding extraction.
#'
#' @param img_path Path to the image file.
#' @param detector Detector to use.
#' @param min_confidence Minimum detection confidence (0-1).
#'
#' @return Logical. TRUE if a valid face is detected.
#'
#' @examples
#' \dontrun{
#' if (has_valid_face("image.jpg")) {
#'   emb <- get_embedding("image.jpg")
#' }
#' }
#'
#' @export
has_valid_face <- function(img_path, 
                           detector = "retinaface",
                           min_confidence = 0.5) {
  
  if (!file.exists(img_path)) {
    return(FALSE)
  }
  
  faces <- detect_faces(img_path, detector = detector)
  
  if (length(faces) == 0) {
    return(FALSE)
  }
  
  # Check if any face meets confidence threshold
  any(sapply(faces, function(f) {
    !is.null(f$confidence) && f$confidence >= min_confidence
  }))
}
