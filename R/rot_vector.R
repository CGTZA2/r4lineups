#'Rotate vector
#'
#'Helper function to rotate vector
#'
#'@param lineup_vec A vector of lineup data
#'@examples
#' # Rotate lineup positions by one place
#' rot_vector(1:6)
#'@importFrom stats lag
#'@export
rot_vector <- function(lineup_vec){
  lineup_vec1 <- lag(lineup_vec)
  lineup_vec1[1] <- lineup_vec[length(lineup_vec)]
  lineup_vec <- lineup_vec1
}
