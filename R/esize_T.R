#' Tredoux Effective Size (Tredoux, 1998)
#'
#' Function for generating Tredoux's transformed index-of-diversity
#' effective size.
#'
#' @param lineup_table A table of lineup choices
#' @details Tredoux (1998) proposed using Agresti and Agresti's (1978)
#'          index of diversity for lineup choices,
#'          \deqn{I = 1 - \frac{\sum_i o_i^2}{N^2},}
#'          where \eqn{o_i} is the observed count for lineup member \eqn{i}
#'          and \eqn{N} is the total number of choices. The index is then
#'          transformed back to an effective-size scale:
#'          \deqn{E' = \frac{1}{1-I} = \frac{N^2}{\sum_i o_i^2}
#'          = \frac{1}{\sum_i p_i^2}.}
#'          This formula is algebraically equivalent to a reciprocal
#'          concentration form.
#' @references Malpass, R. S. (1981). Effective size and defendant bias in eyewitness
#'                  identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'             Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
#'                  qualitative variation. \emph{Sociological Methodology, 9}, 204-237.
#'
#'             Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007).
#'                  Lineup construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read, & M. P. Toglia (Eds.), Handbook of Eyewitness Psychology, Vol. 2: Memory for people (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'             Tredoux, C. G. (1998). Statistical inference on measures of lineup
#'                  fairness. \emph{Law and Human Behavior, 22(2)}, 217-237.
#'
#'             Tredoux, C. (1999). Statistical considerations when determining measures
#'                  of lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'             Wells, G. L., Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'                  empirically assessing the fairness of a lineup. \emph{Law and Human Behavior, 3}(4), 285-293.
#'@examples
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'lineup_table <- table(lineup_vec)
#'
#'#Call:
#'e <- esize_T(lineup_table)
#'
#'@export

esize_T <- function(lineup_table){
  i <- 1-(1/(sum(lineup_table)^2))*sum(lineup_table^2)
  i <- 1/(1-i)
  return(i)
}
