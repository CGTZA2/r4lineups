#'Comparing Effective Size: Base function for bootstrapping
#'
#'A base function for bootstrapping a dataframe of choices for 2 independent lineups
#'
#'@param linedf A dataframe of lineup data. Must consist of 2 columns, each
#'              containing data for 2 independent lineups
#'@param d Indices for bootstrap sample. Argument used by boot function to
#'         select samples for bootstrapping
#'@details The approach here is to compute the effective size of each lineup
#'         separately, and to take the difference between them. This is then
#'         bootstrapped, and if the bootstrap does not contain 0, we
#'          conclude the effective size estimates are different at p = alpha
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
#'
#'            Malpass, R. S. (1981). Effective size and defendant bias in
#'            eyewitness identification lineups. \emph{Law and Human Behavior, 5}(4), 299-309.
#'
#'            Malpass, R. S., Tredoux, C., & McQuiston-Surrett, D. (2007). Lineup
#'            construction and lineup fairness. In R. Lindsay, D. F. Ross, J. D. Read,
#'            & M. P. Toglia (Eds.), \emph{Handbook of Eyewitness Psychology, Vol. 2: Memory for
#'            people} (pp. 155-178). Mahwah, NJ: Lawrence Erlbaum Associates.
#'
#'            Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#'           \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#'            Tredoux, C. (1999). Statistical considerations when determining measures of
#'            lineup size and lineup bias. \emph{Applied Cognitive Psychology, 13}, S9-S26.
#'
#'            Wells, G. L.,Leippe, M. R., & Ostrom, T. M. (1979). Guidelines for
#'            empirically assessing the fairness of a lineup. \emph{Law and Human Behavior,
#'            3}(4), 285-293.
#'@examples
#' # Two independent lineups of 6 members, 50 mock witnesses each
#' set.seed(1)
#' linedf <- data.frame(lineup1 = sample(1:6, 50, replace = TRUE),
#'                      lineup2 = sample(1:6, 50, replace = TRUE))
#' # Difference in effective size for the observed data
#' compare_eff_sizes.boot(linedf, seq_len(nrow(linedf)))
#' # Bootstrap the difference
#' boot_diff <- boot::boot(linedf, compare_eff_sizes.boot, R = 100)
#' boot::boot.ci(boot_diff, type = "perc")
#'@export

compare_eff_sizes.boot <- function(linedf, d){
  temp_df <- linedf[d,]
  esize_T(table(temp_df[[1]])) - esize_T(table(temp_df[[2]]))
}
