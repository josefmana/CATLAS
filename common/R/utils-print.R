#' Print a count as a share of a total
#'
#' @param k Integer(1). The count.
#' @param n Integer(1). The total.
#'
#' @returns Character(1), e.g. `"10 of 16 (62%)"`.
print_n_pct <- function(k, n) {
  sprintf("%d of %d (%.0f%%)", k, n, 100 * k / n)
}


#' Print the exact (Clopper-Pearson) 95% confidence interval of a proportion
#'
#' @inheritParams print_n_pct
#'
#' @returns Character(1), e.g. `"95% CI 35–85%"`.
print_ci_pct <- function(k, n) {
  ci <- stats::binom.test(k, n)$conf.int
  sprintf("95%% CI %.0f\u2013%.0f%%", 100 * ci[1], 100 * ci[2])
}


#' Print minutes with one decimal
#'
#' @param x Numeric. Minutes.
#'
#' @returns Character, as many elements as `x`.
print_min <- function(x) {
  sprintf("%.1f", x)
}


#' Print a whole number in words
#'
#' @param x Integer(1).
#' @param any Logical(1). Spell out any number? With `FALSE` (default), only
#'   numbers below 10 are spelled out, the rest stay as numerals.
#'
#' @returns Character(1), e.g. `"three"`, or `"12"` when `any = FALSE`.
print_num_word <- function(x, any = FALSE) {
  if (any || x < 10) {
    return(as.character(english::english(x)))
  }
  as.character(x)
}


#' Print an estimate with its standard error and confidence interval
#'
#' @param row A data frame with one row and columns `estimate`, `sd`, `ci_low`,
#'   `ci_high` and `perc`.
#'
#' @returns Character(1), e.g. `"M = 2.03, SD = 0.74, 95% CI [0.51, 3.55]"`.
print_estimate <- function(row) {
  stopifnot(nrow(row) == 1L)
  sprintf(
    "*M* = %.2f, *SD* = %.2f, %s%% CI [%.2f, %.2f]",
    row$estimate,
    row$sd,
    row$perc * 100,
    row$ci_low,
    row$ci_high
  )
}


#' Print a one-sample t-test with its standardised effect
#'
#' @param row A data frame with one row and columns `t`, `df`, `p` and `d`.
#'
#' @returns Character(1), e.g. `"t(25) = 2.74, p = .011, d = 0.55"`.
print_test <- function(row) {
  stopifnot(nrow(row) == 1L)
  p <- if (row$p < 0.001) {
    "< .001"
  } else {
    paste("=", sub("^0", "", sprintf("%.3f", row$p)))
  }
  sprintf(
    "*t*(%d) = %.2f, *p* %s, *d* = %.2f",
    row$df,
    row$t,
    p,
    row$d
  )
}
