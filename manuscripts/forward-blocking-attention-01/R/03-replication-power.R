# 03-replication-power.R
#
# Contains all functions bespoke to the `03-replication-power.qmd`
# notebook. More general functions live in `../../../common/R`, `{catlasr}` or
# `{mathpsychr}`, following the rule described in `02-feasibility-study.R`.
#
# The file is split by the notebook sections in a one-to-one manner, with
# subheadings of the file mapping to the `R code` callouts of the text.


# ---- 1 · Effect size of blocking ---------------------------------------------

## ---- 1.1 · Prior literature -------------------------------------------------

#' Confidence interval of Cohen's d_z from an observed t-statistic
#'
#' Inverts the noncentral t distribution of the observed t-statistic, the same
#' computation as `ci_d_one_sample()` in `02-feasibility-study.R`.
#'
#' @param t Numeric(1). The t-statistic of a one-sample or paired t-test.
#' @param n Integer(1). The number of participants.
#' @param level Numeric(1). Confidence level, e.g. `0.90`.
#'
#' @returns Numeric(2), the lower and upper bound.
ci_dz <- function(t, n, level) {
  tail <- (1 - level) / 2
  root <- function(target) {
    suppressWarnings(stats::uniroot(
      \(ncp) stats::pt(t, n - 1L, ncp) - target,
      interval  = c(t - 10, t + 10),
      extendInt = "yes"
    )$root)
  }

  c(root(1 - tail), root(tail)) / sqrt(n)
}


#' Standardise reported tests of blocking
#'
#' Converts each reported one-sample or paired test to Cohen's
#' \eqn{d_z = t / \sqrt{n}}, i.e., the mean (difference) divided by its
#' standard deviation, the effect size the feasibility study reports. F-tests
#' must have one numerator degree of freedom, so that \eqn{t = \sqrt{F}}.
#'
#' @param tests A tibble with one row per reported test and columns `study`,
#'   `test` (`"t"` or `"F"`), `statistic` and `n`.
#' @param level Numeric(1). Confidence level of the intervals, e.g. `0.90`.
#'
#' @returns `tests` with added columns `t`, `df`, `p` (two-sided, as the
#'   original studies tested), `d`, `d_low` and `d_high`.
standardise_reported_tests <- function(tests, level) {
  stopifnot(all(tests$test %in% c("t", "F")))

  out <-
    tests |>
    dplyr::mutate(
      t  = dplyr::if_else(test == "F", sqrt(statistic), statistic),
      df = n - 1L,
      p  = 2 * stats::pt(abs(t), df, lower.tail = FALSE),
      d  = t / sqrt(n)
    )

  ci <- purrr::map2(out$t, out$n, \(t, n) ci_dz(t, n, level))
  out$d_low <- purrr::map_dbl(ci, 1L)
  out$d_high <- purrr::map_dbl(ci, 2L)

  out
}


## ---- 1.2 · Feasibility estimates -------------------------------------------

#' Read an output saved by another notebook of this manuscript
#'
#' Notebooks save the results that later notebooks need to the manuscript's
#' `outputs/` folder (ignored by git) while they render. The file name starts
#' with the number of the notebook that writes it.
#'
#' @param file Character(1). File name within `outputs/`, e.g.
#'   `"02-feasibility-estimates.rds"`.
#' @param repo Character(1). Path to the CATLAS repository.
#'
#' @returns The object stored in `file`.
read_output <- function(file, repo) {
  dir <- file.path(repo, "manuscripts", "forward-blocking-attention-01", "outputs")
  path <- file.path(dir, file)
  if (!file.exists(path)) {
    stop(
      sprintf(
        "'%s' is not in %s. Render notebook %s first, which saves it.",
        file,
        dir,
        sub("-.*$", "", file)
      ),
      call. = FALSE
    )
  }
  readRDS(path)
}


#' Pick one value of the pilot estimates
#'
#' @param estimates The pilot estimates saved by `02-feasibility-study.qmd`, one
#'   row per analysis set and index.
#' @param set Character(1). The analysis set, as named in `estimates$set`.
#' @param index Character(1). `"bi_confidence"` (BI2) or `"bi_choice"` (BI3).
#' @param column Character(1). The column to return, `"d"` by default.
#'
#' @returns The value, of length one.
pick_estimate <- function(estimates, set, index, column = "d") {
  row <- estimates[estimates$set == set & estimates$index == index, ]
  stopifnot(nrow(row) == 1L)
  row[[column]]
}


#' Prepare the pilot effect sizes for their table
#'
#' @inheritParams pick_estimate
#'
#' @returns A tibble with one row per analysis set: `set`, `n`, and per index
#'   (`bi2_`, `bi3_`) the effect size with its CI (`d_ci`) and the one-sided
#'   *p*-value (`p`).
prepare_pilot_estimates <- function(estimates) {
  # Both indices are computed on the same participants of a set
  stopifnot(all(
    estimates |>
      dplyr::group_by(set) |>
      dplyr::summarise(same_n = dplyr::n_distinct(n) == 1L, .groups = "drop") |>
      dplyr::pull(same_n)
  ))

  estimates |>
    dplyr::mutate(
      index = dplyr::if_else(index == "bi_confidence", "bi2", "bi3"),
      d_ci  = paste(print_d(d), print_ci(d_low, d_high)),
      p     = sub("^= ", "", print_p(p))
    ) |>
    dplyr::select(set, n, index, d_ci, p) |>
    tidyr::pivot_wider(
      names_from  = "index",
      values_from = c("d_ci", "p"),
      names_glue  = "{index}_{.value}"
    ) |>
    dplyr::select(set, n, bi2_d_ci, bi2_p, bi3_d_ci, bi3_p) |>
    dplyr::arrange(set)
}


#' Table of the pilot effect sizes
#'
#' @param prepared From [prepare_pilot_estimates()].
#' @param level Numeric(1). Confidence level of the intervals, e.g. `0.90`.
#'
#' @returns A `gt` table.
tbl_pilot_estimates <- function(prepared, level) {
  prepared |>
    gt::gt() |>
    gt::cols_label(
      set      = "Set",
      n        = gt::md("*n*"),
      bi2_d_ci = gt::md(sprintf("*d*~z~ [%.0f%% CI]", 100 * level)),
      bi3_d_ci = gt::md(sprintf("*d*~z~ [%.0f%% CI]", 100 * level)),
      bi2_p    = gt::md("*p*"),
      bi3_p    = gt::md("*p*")
    ) |>
    gt::tab_spanner(
      label   = gt::md("BI~2~ (block 1)"),
      columns = c("bi2_d_ci", "bi2_p")
    ) |>
    gt::tab_spanner(
      label   = gt::md("BI~3~ (blocks 1&ndash;6)"),
      columns = c("bi3_d_ci", "bi3_p")
    ) |>
    gt::cols_align(
      align   = "left",
      columns = "set"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"set"
    ) |>
    gt::tab_source_note(gt::md(paste(
      "*d*~z~: Cohen's d, mean divided by the standard deviation; CI:",
      "two-sided confidence interval computed by inverting the noncentral t",
      "distribution of the observed t-statistic; *p*: one-sided one-sample",
      "t-test against no blocking."
    )))
}


## ---- 1.3 · Retention --------------------------------------------------------

#' Retention through the exclusion steps of the pilot
#'
#' @param funnel The attrition funnel saved by `02-feasibility-study.qmd`, one
#'   row per exclusion step.
#' @param channel Character(1). Column of `funnel` to use: `"Total"` (default)
#'   or a recruitment channel, e.g. `"LABELS"`.
#'
#' @returns A tibble with one row per step that removed any participant (over
#'   all channels): `step`, `before`, `after`, `yield` (share of `before`
#'   retained) and `cumulative` (share of all JATOS records retained).
summarise_retention <- function(funnel, channel = "Total") {
  remaining <- funnel[[channel]]

  tibble::tibble(
    step    = funnel$step,
    removed = funnel$removed,
    before  = dplyr::lag(remaining),
    after   = remaining
  ) |>
    dplyr::filter(!is.na(removed), removed > 0L) |>
    dplyr::mutate(
      yield      = after / before,
      cumulative = after / remaining[1L]
    ) |>
    dplyr::select(-removed)
}


#' Prepare the retention of all channels and of LABELS for their table
#'
#' @inheritParams summarise_retention
#'
#' @returns A tibble with one row per step that removed any participant, plus
#'   a last row with the overall retention: `step`, `all`, `labels`.
prepare_retention <- function(funnel) {
  all <- summarise_retention(funnel, "Total")
  labels <- summarise_retention(funnel, "LABELS")
  stopifnot(identical(all$step, labels$step))

  dplyr::bind_rows(
    tibble::tibble(
      step   = all$step,
      all    = print_n_pct(all$after, all$before),
      labels = print_n_pct(labels$after, labels$before)
    ),
    tibble::tibble(
      step   = "Retained overall",
      all    = print_n_pct(funnel$Total[nrow(funnel)], funnel$Total[1L]),
      labels = print_n_pct(funnel$LABELS[nrow(funnel)], funnel$LABELS[1L])
    )
  )
}


#' Table of the retention of the pilot
#'
#' @param prepared From [prepare_retention()].
#'
#' @returns A `gt` table.
tbl_retention <- function(prepared) {
  prepared |>
    gt::gt() |>
    gt::cols_label(
      step   = "Step",
      all    = "All channels",
      labels = "LABELS"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"step"
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = step == "Retained overall")
    ) |>
    gt::tab_source_note(gt::md(paste(
      "Each cell gives the participants retained by the step out of those it",
      "was applied to. Steps that removed nobody (no jsPsych data, declined",
      "consent) are left out."
    )))
}
