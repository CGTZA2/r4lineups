#'Functional Size with Bootstrapped Confidence Intervals
#'
#'This function is a master function, calling other functions it needs,
#'and reporting results in some detail
#'
#'@param lineup_vec A numeric vector of lineup choices
#'@param target_pos A scalar, representing target position in lineup. Must be declared by user
#'@param R Number of bootstrap samples. Defaults to 1000
#'@param k Number of members in lineup. Must be specified by user (scalar).
#'@return Invisibly returns the functional-size estimate, normal, percentile,
#'  and BCa intervals, the number of replications, and the bootstrap object.
#'@seealso \code{\link[boot:boot]{boot}}: https://cran.r-project.org/web/packages/boot/boot.pdf
#'@details Function depends on functions from package 'boot'
#'@references Davison,  A.C. & Hinkley,  D.V. (1997). \emph{Bootstrap methods and their
#'            application}. Cambridge University Press.
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
#'#Data:
#'lineup_vec <- round(runif(100, 1, 6))
#'target_pos <- 3
#'
#'#Call:
#'x <- func_size_report(lineup_vec, target_pos, 6)
#'x <- func_size_report(lineup_vec, 3, 6)
#'
#'@export
#'@importFrom boot boot boot.ci

func_size_report <- function(lineup_vec, target_pos, k, R = 1000){
  lineup_vec <- typecheck(lineup_vec)
  datacheck1(lineup_vec, k)
  if (!is.numeric(R) || length(R) != 1L || R < 1 || R != as.integer(R)) {
    stop("R must be a positive integer.", call. = FALSE)
  }
  estimate <- suppressWarnings(func_size(lineup_vec, target_pos))
  temp1 <- boot(lineup_vec, func_size.boot, target_pos = target_pos, R = R)
  temp2 <- suppressWarnings(boot.ci(
    temp1, type = c("norm", "bca", "perc"), target_pos = target_pos
  ))
  normal <- if (!is.null(temp2$normal)) temp2$normal[2:3] else c(NA_real_, NA_real_)
  percentile <- if (!is.null(temp2$percent)) temp2$percent[4:5] else c(NA_real_, NA_real_)
  bca <- if (!is.null(temp2$bca)) temp2$bca[4:5] else c(NA_real_, NA_real_)

  cat("Functional size of lineup is ", round(estimate, 3), "\n", sep = "")
  cat("Confidence intervals [95%]\n")
  cat("Normal Theory", round(normal, 3), "\n")
  cat(sprintf("Bootstrap: percentile (R = %d)", R), round(percentile, 3), "\n")
  cat(sprintf("Bootstrap: bias-corrected (R = %d)", R), round(bca, 3), "\n")

  invisible(list(
    estimate = estimate,
    normal = normal,
    percentile = percentile,
    bca = bca,
    R = as.integer(R),
    boot = temp1
  ))
}
