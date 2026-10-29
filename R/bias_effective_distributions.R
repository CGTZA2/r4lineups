#' Bootstrap Distribution of Lineup Bias (Target Proportion)
#'
#' Computes a bootstrap distribution for the target-position proportion.
#'
#' @param lineup_input A numeric lineup vector or a lineup table.
#' @param target_pos Target position in the lineup (scalar).
#' @param k Nominal lineup size (suspect + fillers).
#' @param input_type Either "vector" or "table".
#' @param R Number of bootstrap resamples.
#'
#' @return A numeric vector of bootstrap estimates, carrying an \code{"n"}
#'   attribute equal to the number of lineup choices resampled. Because each
#'   estimate is a count divided by \code{n}, the estimates fall on a discrete
#'   lattice with spacing \code{1/n}; \code{\link{plot_lineup_bias_distribution}}
#'   uses the attribute to bin at that resolution.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
lineup_bias_boot_dist <- function(lineup_input,
                                  target_pos,
                                  k,
                                  input_type = c("vector", "table"),
                                  R = 1000) {
  input_type <- match.arg(input_type)
  if (length(target_pos) != 1 || !is.finite(target_pos)) {
    stop("target_pos must be a finite scalar.", call. = FALSE)
  }
  if (length(k) != 1 || !is.finite(k) || k < 2) {
    stop("k must be a finite scalar >= 2.", call. = FALSE)
  }
  k <- as.integer(k)
  target_pos <- as.integer(target_pos)
  if (target_pos < 1 || target_pos > k) {
    stop("target_pos must be between 1 and k.", call. = FALSE)
  }

  vec <- msdt_make_lineup_vector(lineup_input, k, input_type)
  if (length(vec) < 2) {
    stop("Need at least two selections for bootstrap.", call. = FALSE)
  }
  out <- as.vector(boot::boot(vec, lineup_prop_boot,
                              target_pos = target_pos, R = R)$t)
  attr(out, "n") <- length(vec)
  out
}

#' Plot Bootstrap Distribution of Lineup Bias
#'
#' @param boot_values Numeric vector of bootstrap estimates, as returned by
#'   \code{\link{lineup_bias_boot_dist}}.
#' @param target_pos Optional target position label for title.
#' @param bins Number of histogram bins. Defaults to \code{NULL}, which selects
#'   the bin width automatically (see Details).
#' @param binwidth Histogram bin width. Defaults to \code{NULL}, which selects
#'   the bin width automatically (see Details). Takes precedence over
#'   \code{bins}.
#'
#' @details Bootstrap estimates of a lineup proportion are counts divided by the
#'   number of lineup choices, so they fall on a discrete lattice with spacing
#'   \code{1/n} rather than on a continuum. Binning such values at an arbitrary
#'   resolution produces a misleading histogram: bins that contain no attainable
#'   value are empty regardless of the data, and bins that happen to straddle two
#'   attainable values are twice as tall as their neighbours. When neither
#'   \code{bins} nor \code{binwidth} is supplied, the lattice spacing is
#'   recovered (from the \code{"n"} attribute when present, otherwise from the
#'   values themselves) and used as the bin width, so each bar covers exactly one
#'   attainable proportion. Estimates that are effectively continuous fall back
#'   to 30 bins.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
#' plot_lineup_bias_distribution(dist, target_pos = 3)
plot_lineup_bias_distribution <- function(boot_values, target_pos = NULL,
                                          bins = NULL, binwidth = NULL) {
  values <- as.vector(boot_values)
  df <- data.frame(value = values)
  title <- "Bootstrap distribution of target proportion"
  if (!is.null(target_pos)) {
    title <- paste0(title, " (target position ", target_pos, ")")
  }
  hist_layer <- boot_hist_layer(values, bins = bins, binwidth = binwidth,
                                n = attr(boot_values, "n"))
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    hist_layer +
    ggplot2::geom_density(color = "#1f2d3d", linewidth = 1) +
    ggplot2::labs(x = "Target proportion", y = "Density", title = title) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Bootstrap Distribution of Effective Size
#'
#' Computes a bootstrap distribution for effective size using either
#' Tredoux's E' or Malpass's adjusted E.
#'
#' @param lineup_input A numeric lineup vector or a lineup table.
#' @param k Nominal lineup size (suspect + fillers).
#' @param metric Either "tredoux" or "malpass".
#' @param input_type Either "vector" or "table".
#' @param R Number of bootstrap resamples.
#' @param unbiased Logical. For \code{metric = "tredoux"}, bootstrap the
#'   small-sample corrected E' of \code{\link{esize_T}(unbiased = TRUE)}
#'   instead of the plug-in estimate. Default \code{FALSE}.
#'
#' @return A numeric vector of bootstrap estimates, carrying an \code{"n"}
#'   attribute equal to the number of lineup choices resampled. With
#'   \code{unbiased = TRUE}, resamples in which every choice fell on a
#'   different member have an unbounded corrected estimate; they are dropped,
#'   with a warning, and their number is recorded in an \code{"n_undefined"}
#'   attribute.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
#' dist_corrected <- esize_boot_dist(vec, k = 6, R = 500, unbiased = TRUE)
esize_boot_dist <- function(lineup_input,
                            k,
                            metric = c("tredoux", "malpass"),
                            input_type = c("vector", "table"),
                            R = 1000,
                            unbiased = FALSE) {
  metric <- match.arg(metric)
  input_type <- match.arg(input_type)
  if (!is.logical(unbiased) || length(unbiased) != 1L || is.na(unbiased)) {
    stop("unbiased must be TRUE or FALSE.", call. = FALSE)
  }
  if (unbiased && metric == "malpass") {
    stop("unbiased applies only to metric = \"tredoux\".", call. = FALSE)
  }
  if (length(k) != 1 || !is.finite(k) || k < 2) {
    stop("k must be a finite scalar >= 2.", call. = FALSE)
  }
  k <- as.integer(k)

  vec <- msdt_make_lineup_vector(lineup_input, k, input_type)
  if (length(vec) < 2) {
    stop("Need at least two selections for bootstrap.", call. = FALSE)
  }

  if (metric == "tredoux" && unbiased) {
    # The corrected estimator directly, so that an unbounded resample gives NA
    # rather than a per-resample warning and the plug-in value
    boot_fun <- function(lineup_vec, d) {
      counts <- as.numeric(table(factor(lineup_vec[d], levels = seq_len(k))))
      n <- sum(counts)
      concentration <- sum(counts * (counts - 1))
      if (concentration > 0) n * (n - 1) / concentration else NA_real_
    }
  } else if (metric == "tredoux") {
    boot_fun <- function(lineup_vec, d) {
      boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
      esize_T(boot_tab)
    }
  } else {
    boot_fun <- function(lineup_vec, d) {
      boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
      esize_m(boot_tab, k)
    }
  }

  out <- as.vector(boot::boot(vec, boot_fun, R = R)$t)
  if (unbiased) {
    undefined <- sum(is.na(out))
    if (undefined > 0) {
      warning(undefined, " of ", R, " resamples had every choice on a different ",
              "member, so the corrected estimate is unbounded; they are excluded.",
              call. = FALSE)
      out <- out[!is.na(out)]
    }
    attr(out, "n_undefined") <- undefined
  }
  attr(out, "n") <- length(vec)
  out
}

#' Plot Bootstrap Distribution of Effective Size
#'
#' @param boot_values Numeric vector of bootstrap estimates, as returned by
#'   \code{\link{esize_boot_dist}}.
#' @param metric Label for the effective size statistic.
#' @param bins Number of histogram bins. Defaults to \code{NULL}, which selects
#'   the bin width automatically.
#' @param binwidth Histogram bin width. Defaults to \code{NULL}, which selects
#'   the bin width automatically. Takes precedence over \code{bins}.
#'
#' @details Effective size is a function of the whole table of choices, so its
#'   bootstrap distribution is usually fine-grained enough to bin as a continuous
#'   quantity, and the default is then 30 bins. When the number of lineup choices
#'   is small the estimates become visibly discrete, and the automatic rule bins
#'   at the resolution of the attainable values instead. See
#'   \code{\link{plot_lineup_bias_distribution}} for the rationale.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
#' plot_esize_distribution(dist, metric = "E (Tredoux, 1998)")
plot_esize_distribution <- function(boot_values, metric = "Effective size",
                                    bins = NULL, binwidth = NULL) {
  values <- as.vector(boot_values)
  df <- data.frame(value = values)
  hist_layer <- boot_hist_layer(values, bins = bins, binwidth = binwidth)
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    hist_layer +
    ggplot2::geom_density(color = "#1f2d3d", linewidth = 1) +
    ggplot2::labs(x = metric, y = "Density",
                  title = "Bootstrap distribution of effective size") +
    ggplot2::theme_minimal(base_size = 12)
}

# Bootstrap estimates that are counts over a fixed denominator fall on a regular
# lattice rather than on a continuum. Returns the lattice spacing, to be used as
# a histogram bin width, or NULL when the values are fine-grained enough to treat
# as continuous. `n`, when supplied, is the resampled sample size, which fixes
# the spacing exactly at 1/n rather than inferring it from the observed values.
boot_lattice_binwidth <- function(v, n = NULL, max_bins = 100) {
  v <- as.vector(v)
  v <- v[is.finite(v)]
  u <- sort(unique(v))
  if (length(u) < 3) {
    return(NULL)
  }

  if (!is.null(n) && is.finite(n) && n > 0) {
    step <- 1 / n
  } else {
    step <- min(diff(u))
  }
  if (!is.finite(step) || step <= 0) {
    return(NULL)
  }

  # Every observed value must sit on the lattice, and the lattice must be coarse
  # enough that one bar per attainable value stays readable.
  offsets <- (u - u[1]) / step
  if (any(abs(offsets - round(offsets)) > 1e-6)) {
    return(NULL)
  }
  if (round(diff(range(u)) / step) + 1 > max_bins) {
    return(NULL)
  }

  step
}

# Shared histogram layer for the bootstrap distribution plots. Honours an
# explicit `binwidth` or `bins`, otherwise bins on the lattice when there is one
# and falls back to 30 bins when there is not.
boot_hist_layer <- function(values, bins = NULL, binwidth = NULL, n = NULL) {
  centre <- NULL
  if (!is.null(binwidth)) {
    bins <- NULL
  }
  if (is.null(binwidth) && is.null(bins)) {
    binwidth <- boot_lattice_binwidth(values, n = n)
    if (is.null(binwidth)) {
      bins <- 30
    } else {
      centre <- min(values)
    }
  }

  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins = bins, binwidth = binwidth, center = centre,
    fill = "#72b7b2", alpha = 0.6
  )
}

msdt_make_lineup_vector <- function(lineup_input, k, input_type) {
  if (input_type == "vector") {
    if (is.data.frame(lineup_input) || is.matrix(lineup_input)) {
      vec <- lineup_input[[1]]
    } else {
      vec <- lineup_input
    }
    if (!is.numeric(vec)) {
      vec <- as.numeric(vec)
    }
    vec <- vec[!is.na(vec)]
    if (any(vec %% 1 != 0)) {
      stop("Lineup vector must use integer positions.", call. = FALSE)
    }
    if (any(vec < 1 | vec > k)) {
      stop("Lineup vector values must be between 1 and k.", call. = FALSE)
    }
    return(as.integer(vec))
  }
  tab <- as.numeric(lineup_input)
  if (length(tab) != k) {
    stop("Lineup table length must equal k.", call. = FALSE)
  }
  if (any(tab < 0)) {
    stop("Lineup table cannot include negative counts.", call. = FALSE)
  }
  rep(seq_len(k), tab)
}
