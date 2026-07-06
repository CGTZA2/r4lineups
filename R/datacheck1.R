#'Helper function
#'
#'Checks that number of lineup choices contained in a vector is accurate
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param k Number of members in lineup
#'@details This function ensures that a non-selected lineup member is not accidentally
#'         omitted from the dataframe due to lack of selection by all mock witnesses.
#'         It functions as a check that the total number of lineup members is accurate.
#'@examples
#' # Choices of 50 mock witnesses to a 6-member lineup: passes silently
#' set.seed(1)
#' lineup_vec <- sample(1:6, 50, replace = TRUE)
#' datacheck1(lineup_vec, 6)
#'@export

datacheck1 <- function(lineup_vec, k){
  if (length(table(lineup_vec))== k){
    lineup_vec = lineup_vec
  }
  else{
    stop("User-declared nominal size does not match observed nominal size. Please
         check vector of target positions.")
  }
}
