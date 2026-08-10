#' Master function: Homogeneity of diagnosticity ratio
#'
#' This function provides assesses the homogeneity of the diagnosticity ratio of
#'  k lineup pairs.
#'
#'@param lineup_pres_list A list containing k vectors of lineup choices for k lineups, in which the
#'                        target was present
#'@param lineup_abs_list A list containing k vectors of lineup choices for k lineups, in which the
#'                       target was absent
#'@param pos_list Suspect positions for each lineup pair. See \code{diag_param()}.
#'@param k A vector indexing number of members in each lineup pair (nominal size). Must be specified by user (scalar).
#'@return Computes diagnosticity ratio with chi-squared estimate and significance
#'         level for k lineup pairs
#'@details Master function for assessing homogeneity of diagnosticity ratio for
#'         k independent lineups.
#'@references Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'            \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology}, 13, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#'#Target present data:
#'A <- rep(1:6, length.out = 100)
#'B <- rep(1:5, length.out = 70)
#'C <- rep(1:4, length.out = 20)
#'lineup_pres_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'#Target absent data:
#'A <- rep(6:1, length.out = 100)
#'B <- rep(5:1, length.out = 70)
#'C <- rep(4:1, length.out = 20)
#'lineup_abs_list <- list(A, B, C)
#'rm(A, B, C)
#'
#'# Suspect position for each TP/TA pair
#'pos_list <- c(3, 2, 1)
#'
#'#Nominal size:
#'k <- c(6, 5, 4)
#'
#'#Call:
#'homog_diag(lineup_pres_list, lineup_abs_list, pos_list, k)
#'
#'@export

homog_diag <- function(lineup_pres_list, lineup_abs_list, pos_list, k){
  linedf <- suppressWarnings(diag_param(lineup_pres_list, lineup_abs_list, pos_list, k))
  par1 <- var_lnd(linedf)
  par2 <- ln_diag_ratio(linedf, correction = FALSE)
  par3 <- d_weights(linedf)
  par4 <- t(cbind(par1, par2, par3))
  par5 <- chi_diag(par4)
  df <- nrow(linedf) - 1L
  par6 <- pchisq(par5, df = df, lower.tail = FALSE)
  par7 <- d_bar(par4)
  cat("Mean diagnosticity ratio:", par7)
  cat("\n")
  cat("Chi-square estimate (q):", par5)
  cat("\n")
  cat("Sig:", par6)
  invisible(list(
    mean_diagnosticity = par7,
    chi_square = par5,
    df = df,
    p_value = par6,
    parameters = linedf
  ))
}
