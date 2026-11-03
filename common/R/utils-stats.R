#' Exact confidence interval of a one-sample standardised mean
#'
#' Inverts the noncentral t distribution of the observed t-statistic, so the
#' interval stays exact in small samples.
#'
#' @param t Numeric(1). The one-sample t statistic.
#' @param n Integer(1). The number of participants.
#' @param level Numeric(1). Confidence level.
#'
#' @returns Numeric(2), the bounds of Cohen's \eqn{d_z}; `NA` when `t` is
#'   missing or `n` is below 2.
#' 
#' @references
#' Steiger, J. H., & Fouladi, R. T. (1997). Noncentrality interval estimation
#'   and the evaluation of statistical models. In L. L. Harlow, S. A. Mulaik,
#'   & J. H. Steiger (Eds.), *What if there were no significance tests?*
#'   (pp. 221-257). Lawrence Erlbaum.
#'
#' Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
#'   calculation of confidence intervals that are based on central and
#'   noncentral distributions. *Educational and Psychological Measurement,
#'   61*(4), 532-574. \doi{10.1177/0013164401614002}
ci_d_one_sample <- function(t, n, level = 0.95) {
  if (is.na(t) || n < 2L) {
    return(c(NA_real_, NA_real_))
  }
  
  tail <- (1 - level) / 2
  gap <- function(ncp, target) {
    stats::pt(t, n - 1L, ncp) - target
  }
  root <- function(target) {
    suppressWarnings(stats::uniroot(
      gap,
      interval  = c(t - 10, t + 10),
      target    = target,
      extendInt = "yes"
    )$root)
  }
  
  c(root(1 - tail), root(tail)) / sqrt(n)
}
