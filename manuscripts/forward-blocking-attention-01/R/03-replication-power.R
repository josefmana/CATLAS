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
#' standard deviation. F-tests must have one numerator degree of freedom, so
#' that \eqn{t = \sqrt{F}}.
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
    dplyr::select(
      set,
      n,
      index,
      d_ci,
      p
    ) |>
    tidyr::pivot_wider(
      names_from  = "index",
      values_from = c("d_ci", "p"),
      names_glue  = "{index}_{.value}"
    ) |>
    dplyr::select(
      set,
      n,
      bi2_d_ci, bi2_p,
      bi3_d_ci, bi3_p
    ) |>
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
      "distribution of the observed *t*-statistic; *p*: one-sided one-sample",
      "*t*-test against no blocking."
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
    dplyr::filter(
      !is.na(removed),
      removed > 0L
    ) |>
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
      "Each cell shows the number of participants retained by the step and",
      "their percentage out of those the rule was applied to. Steps that did",
      "not remove anybody (no jsPsych data, declined consent) are left out."
    )))
}


# ---- 2 · Prospective power analysis ------------------------------------------

## ---- 2.1 · Sample size -------------------------------------------------------

#' Sample size of a one-sided one-sample t-test, exact
#'
#' Searches the smallest number of participants at which the one-sided
#' one-sample t-test of a standardised mean `d` (Cohen's \eqn{d_z}) reaches the
#' target power, using the noncentral t distribution. It serves as an
#' independent check of the `{pwrss}` results.
#'
#' @param d Numeric(1). The true standardised mean, \eqn{d_z > 0}.
#' @param power Numeric(1). The target power, e.g. `0.90`.
#' @param alpha Numeric(1). The one-sided significance level, e.g. `0.05`.
#'
#' @returns Integer(1), the number of participants analysed.
n_one_sample_t <- function(d, power, alpha) {
  stopifnot(d > 0)
  achieved <- function(n) {
    stats::pt(
      stats::qt(1 - alpha, n - 1),
      df = n - 1,
      ncp = d * sqrt(n),
      lower.tail = FALSE
    )
  }
  n <- 2L
  while (achieved(n) < power) n <- n + 1L
  n
}


#' Pick one sample size
#'
#' @param sizes The sample sizes per scenario and target power, with columns
#'   `scenario`, `power`, `n` and `recruit`.
#' @param scenario Character(1). `"worst"`, `"realistic"` or `"best"`.
#' @param power Numeric(1). The target power, e.g. `0.90`.
#' @param column Character(1). `"n"` (analysed, default) or `"recruit"`.
#'
#' @returns The value, of length one.
pick_size <- function(sizes, scenario, power, column = "n") {
  row <- sizes[sizes$scenario == scenario & abs(sizes$power - power) < 1e-9, ]
  stopifnot(nrow(row) == 1L)
  row[[column]]
}


#' Pick the power of a scenario at a sample size sized for *Realistic*
#'
#' @param cross The power per scenario at the sample sizes sized for the
#'   *Realistic* scenario, with columns `target` (the power that set `n`),
#'   `scenario`, `n`, `d` and `power`.
#' @param scenario Character(1). `"worst"` or `"best"`.
#' @param target Numeric(1). The target power that set the sample size, e.g.
#'   `0.90`.
#'
#' @returns Numeric(1), the power.
pick_power <- function(cross, scenario, target) {
  row <- cross[cross$scenario == scenario & abs(cross$target - target) < 1e-9, ]
  stopifnot(nrow(row) == 1L)
  row$power
}


#' Table of the sample sizes per scenario
#'
#' @param sizes The sample sizes per scenario and target power, with columns
#'   `scenario`, `d`, `power`, `n` and `recruit`.
#' @param scenario_labels Named character. A label per scenario.
#' @param retention Numeric(1). The assumed retention, e.g. `0.50`.
#' @param alpha Numeric(1). The one-sided significance level.
#' @param chosen Character(1). Selected scenario name. Its row will be
#'   highlighted in bold. `NULL` for no highlighting (default).
#'
#' @returns A `gt` table with one row per scenario and one column per target
#'   power, each cell giving the analysed and (in brackets) recruited sample.
tbl_sample_sizes <- function(
    sizes,
    scenario_labels,
    retention,
    alpha,
    chosen = NULL
) {
  chosen_note <- if (rlang::is_empty(chosen)) {
    ""
  } else {
    " The scenario on which the sampling decisions are basen is printed in bold."
  }
  
  sizes |>
    dplyr::mutate(
      scenario = scenario_labels[as.character(scenario)],
      cell     = sprintf("%d (%d)", n, recruit),
      power    = sprintf("%.0f%%", 100 * power)
    ) |>
    dplyr::select(
      scenario,
      d,
      power,
      cell
    ) |>
    tidyr::pivot_wider(
      names_from  = "power",
      values_from = "cell"
    ) |>
    gt::gt() |>
    gt::fmt_number(
      columns  = "d",
      decimals = 2
    ) |>
    gt::cols_label(
      scenario = "Scenario",
      d        = gt::md("*d*~z~")
    ) |>
    gt::tab_spanner(
      label   = "Power",
      columns = -c("scenario", "d")
    ) |>
    gt::cols_align(
      align   = "left",
      columns = "scenario"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"scenario"
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = vapply(scenario, \(x) identical(x, chosen), logical(1L)))
    ) |>
    gt::tab_source_note(gt::md(sprintf(
      paste(
        "Participants analysed in the Naive set, with the number to recruit at",
        "%.0f%% retention in brackets, for a one-sided one-sample *t*-test of",
        "BI~2~ on the first test block at &alpha; = %s. Computed by",
        "`pwrss::power.t.student()` and checked against the exact noncentral",
        "*t* distribution.%s"
      ),
      100 * retention,
      sub("^0", "", format(alpha)),
      chosen_note
    )))
}


#' Figure of the sample size required across effect sizes
#'
#' Draws the number of participants needed for each target power as a function
#' of the true effect size (log-scaled sample size) and marks the sample sizes
#' of the three scenarios, which are named on the top axis.
#'
#' @param curve A tibble with columns `d`, `power` (target, e.g. `0.90`) and
#'   `n` (not rounded up).
#' @param points The sample sizes per scenario and target power, with columns
#'   `scenario`, `d`, `power` and `n`.
#' @param scenario_labels Named character. A label per scenario.
#' @param retention Numeric(1). The assumed retention, for the secondary axis
#'   of participants to recruit.
#'
#' @returns A `ggplot`.
fig_power_curve <- function(curve, points, scenario_labels, retention) {
  power_labels <- function(p) sprintf("%.0f%%", 100 * p)
  colours <- c("#56B4E9", "#0072B2", "#000000")
  names(colours) <- power_labels(sort(unique(curve$power)))

  scenario_d <- dplyr::distinct(points, scenario, d)

  ggplot2::ggplot(curve) +
    ggplot2::aes(
      x      = d,
      y      = n,
      colour = power_labels(power)
    ) +
    ggplot2::geom_vline(
      data     = scenario_d,
      mapping  = ggplot2::aes(xintercept = d),
      linetype = "dashed",
      colour   = "grey60"
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(
      data    = points,
      mapping = ggplot2::aes(colour = power_labels(power)),
      size    = 2
    ) +
    ggplot2::geom_label(
      data          = points,
      mapping       = ggplot2::aes(label = n, colour = power_labels(power)),
      hjust         = -0.25,
      size          = 3,
      fill          = "white",
      linewidth     = 0,
      label.padding = ggplot2::unit(0.1, "lines"),
      show.legend   = FALSE
    ) +
    ggplot2::scale_y_log10(
      name     = "Participants analysed",
      breaks   = c(10, 20, 50, 100, 200, 500, 1000, 2000),
      sec.axis = ggplot2::sec_axis(
        ~ . / retention,
        name   = sprintf("Participants to recruit (%.0f%% retention)", 100 * retention),
        breaks = c(20, 50, 100, 200, 500, 1000, 2000, 4000)
      )
    ) +
    ggplot2::scale_x_continuous(
      name     = expression("True effect size" ~ (italic(d)[z])),
      breaks   = seq(0.1, 0.8, by = 0.1),
      sec.axis = ggplot2::dup_axis(
        name   = NULL,
        breaks = scenario_d$d,
        labels = sprintf(
          "%s (%.2f)",
          scenario_labels[as.character(scenario_d$scenario)],
          scenario_d$d
        )
      )
    ) +
    ggplot2::scale_colour_manual(
      name   = "Power",
      values = colours
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}


## ---- 2.2 · Power of the alternative index -----------------------------------

#' Table of the power of the alternative index
#'
#' @param bi3_power A tibble with columns `target` (the BI2 target power that
#'   set `n`), `n`, `set` (the analysis set whose BI3 estimate is assumed), `d`
#'   and `power`.
#' @param alpha Numeric(1). The one-sided significance level.
#'
#' @returns A `gt` table with one row per sample size and one column per
#'   assumed BI3 effect size.
tbl_bi3_power <- function(bi3_power, alpha) {
  bi3_power |>
    dplyr::mutate(
      target = sprintf("%.0f%%", 100 * target),
      column = sprintf("%s (%.2f)", set, d),
      power  = sprintf("%.0f%%", 100 * power)
    ) |>
    dplyr::select(target, n, column, power) |>
    tidyr::pivot_wider(
      names_from  = "column",
      values_from = "power"
    ) |>
    gt::gt() |>
    gt::cols_label(
      target = gt::md("BI~2~ power"),
      n      = gt::md("*n*")
    ) |>
    gt::tab_spanner(
      label   = gt::md("BI~3~ power if *d*~z~ equals the estimate of the"),
      columns = -c("target", "n")
    ) |>
    gt::cols_align(
      align   = "center",
      columns = tidyselect::everything()
    ) |>
    gt::tab_source_note(gt::md(sprintf(
      paste(
        "Power of the one-sided one-sample *t*-test of BI~3~ over test blocks",
        "1&ndash;6 at &alpha; = %s, at the sample sizes that give BI~2~ the",
        "listed power under the *Realistic* scenario. Computed by",
        "`pwrss::power.t.student()`."
      ),
      sub("^0", "", format(alpha))
    )))
}
