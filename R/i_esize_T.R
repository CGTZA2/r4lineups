#' I Component of Tredoux Effective Size (Tredoux, 1998)
#'
#' Computes the Agresti-Agresti index of diversity I, the intermediate quantity
#' used to construct Tredoux's effective size E' = 1/(1 - I).
#'
#' @param lineup_table A table of lineup choices.
#' @details
#'   Tredoux (1998) used Agresti and Agresti's (1978) index of diversity:
#'   \deqn{I = 1 - \frac{\sum_i o_i^2}{N^2},}
#'   where \eqn{o_i} is the observed count for lineup member \eqn{i} and
#'   \eqn{N} is the total number of choices. This equals \eqn{1 - \sum_i p_i^2},
#'   where \eqn{p_i = o_i/N}. The effective size is the reciprocal transform
#'   \eqn{E' = 1/(1-I)}, computed by \code{\link{esize_T}}.
#' @return A scalar: the diversity index I.
#' @references
#'   Agresti, A., & Agresti, B. F. (1978). Statistical analysis of qualitative
#'   variation. \emph{Sociological Methodology, 9}, 204-237.
#'
#'   Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'   \emph{Law and Human Behavior, 22}(2), 217-237.
#' @seealso \code{\link{esize_T}}
#' @examples
#' lineup_vec <- round(runif(100, 1, 6))
#' lineup_table <- table(lineup_vec)
#' i <- i_esize_T(lineup_table)
#' @export

i_esize_T <- function(lineup_table){
  i <- 1-(1/(sum(lineup_table)^2))*sum(lineup_table^2)
  return(i)
}
