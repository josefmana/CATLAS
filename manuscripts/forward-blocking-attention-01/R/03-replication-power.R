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


## ---- 2.3 · Group-sequential top-up --------------------------------------------

#' False-positive rate of testing twice at the full alpha
#'
#' The probability that a one-sided test at `alpha` rejects at look 1 or at
#' look 2 when there is no effect, if both looks used the critical value of a
#' single test. The test statistics of the two looks are correlated by
#' \eqn{\sqrt{n_1 / n_2}} (normal approximation).
#'
#' @param n1,n2 Numeric(1). Participants analysed at looks 1 and 2 (`n2`
#'   cumulative).
#' @param alpha Numeric(1). The one-sided significance level.
#'
#' @returns Numeric(1), the false-positive rate.
alpha_two_looks <- function(n1, n2, alpha) {
  rho <- sqrt(n1 / n2)
  crit <- stats::qnorm(1 - alpha)
  neither <- stats::integrate(
    \(z) stats::dnorm(z) * stats::pnorm((crit - rho * z) / sqrt(1 - rho^2)),
    lower = -Inf,
    upper = crit
  )$value
  1 - neither
}



#' Two-look group-sequential design of the replication
#'
#' The replication proper is look 1; look 2 is the pre-registered top-up. With
#' `spending = "fixed"`, look 1 spends `alpha1` whatever share of the final
#' sample it contains, and look 2 spends the rest.
#'
#' @param n1,n2 Numeric(1). Participants analysed at looks 1 and 2 (`n2` is
#'   the cumulative total).
#' @param alpha Numeric(1). The overall one-sided significance level.
#' @param spending Character(1). `"fixed"`, `"asP"` or `"asOF"`.
#' @param alpha1 Numeric(1). The alpha spent at look 1 with
#'   `spending = "fixed"`.
#'
#' @returns An `{rpact}` design object.
design_topup <- function(
    n1, n2,
    alpha,
    spending = c("fixed", "asP", "asOF"),
    alpha1 = NULL
) {
  spending <- match.arg(spending)
  stopifnot(n1 < n2)

  if (spending == "fixed") {
    stopifnot(!is.null(alpha1), alpha1 < alpha)
    return(rpact::getDesignGroupSequential(
      kMax              = 2L,
      alpha             = alpha,
      sided             = 1L,
      informationRates  = c(n1 / n2, 1),
      typeOfDesign      = "asUser",
      userAlphaSpending = c(alpha1, alpha)
    ))
  }

  rpact::getDesignGroupSequential(
    kMax             = 2L,
    alpha            = alpha,
    sided            = 1L,
    informationRates = c(n1 / n2, 1),
    typeOfDesign     = spending
  )
}


#' Power of a two-look design for a one-sample t-test
#'
#' @param design From [design_topup()].
#' @param n2 Numeric(1). Participants analysed at look 2 (cumulative).
#' @param d Numeric(1). The true standardised mean, \eqn{d_z}.
#'
#' @returns Named numeric: `look1` (probability of rejecting at look 1) and
#'   `overall` (probability of rejecting at either look).
power_topup <- function(design, n2, d) {
  r <- rpact::getPowerMeans(
    design,
    groups               = 1L,
    thetaH0              = 0,
    alternative          = d,
    stDev                = 1,
    maxNumberOfSubjects  = n2,
    normalApproximation  = FALSE
  )

  c(
    look1   = unname(r$rejectPerStage[1L, 1L]),
    overall = unname(r$overallReject[1L])
  )
}


#' Calibrate look 2 sample size of the top-up
#'
#' Searches (by bisection) the smallest cumulative sample at look 2 that gives
#' the design the target overall power.
#'
#' @inheritParams design_topup
#' @param d Numeric(1). The true standardised mean that look 2 is sized for.
#' @param power Numeric(1). The target overall power, e.g. `0.80`.
#' @param upper Numeric(1). Upper end of the search.
#'
#' @returns Integer(1), the participants analysed at look 2 (cumulative).
size_topup <- function(
    n1,
    alpha,
    spending,
    alpha1 = NULL,
    d,
    power,
    upper = 5000L
) {
  achieved <- function(n2) {
    design <- design_topup(n1, n2, alpha, spending, alpha1)
    power_topup(design, n2, d)[["overall"]]
  }
  stopifnot(achieved(upper) >= power)

  lo <- n1 + 1L
  hi <- upper
  while (lo < hi) {
    mid <- (lo + hi) %/% 2L
    if (achieved(mid) >= power) hi <- mid else lo <- mid + 1L
  }
  as.integer(lo)
}


#' Table of the top-up at a range of retention rates
#'
#' @param by_retention From [topup_by_retention()], with the look-1 power under
#'   the *Realistic* and *Worst* scenarios.
#' @param recruit1 Numeric(1). Participants recruited for look 1.
#'
#' @returns A `gt` table with one row per retention rate: the Naive set at each
#'   look, the nominal one-sided significance levels, the look-1 power under
#'   the *Realistic* and *Worst* scenarios, and the participants recruited.
tbl_topup_thresholds <- function(by_retention, recruit1, total_power) {
  alpha1 <- unique(by_retention$p1)
  stopifnot("Look 1 must have a single alpha level." = length(alpha1) == 1L)
  
  overall_power <- if (total_power < 1L) {
    sprintf("%d%%", 100 * total_power)
  } else {
    sprintf("%d%%", total_power)
  }
  
  by_retention |>
    dplyr::mutate(
      retention = sprintf("%.0f%%", 100 * retention),
      p1        = paste("<", sub("^0", "", sprintf("%.3f", p1))),
      p2        = paste("<", sub("^0", "", sprintf("%.4f", p2))),
      dplyr::across(
        c("look1_realistic", "look1_worst"),
        print_power
      ),
      look2_power = overall_power
    ) |>
    dplyr::select(
      retention,
      n1, p1,
      n2, p2,
      look1_realistic, look1_worst,
      look2_power,
      recruit_look1, recruit_extra, recruit_total
    ) |>
    gt::gt() |>
    gt::tab_spanner(
      label   = gt::md("Look 1"),
      columns = c("n1", "p1")
    ) |>
    gt::tab_spanner(
      label   = gt::md("Look 2"),
      columns = c("n2", "p2")
    ) |>
    gt::tab_spanner(
      label   = "Power (look 1)",
      columns = c("look1_realistic", "look1_worst")
    ) |>
    gt::tab_spanner(
      label   = "Recruited",
      columns = c("recruit_look1", "recruit_extra", "recruit_total")
    ) |>
    gt::cols_align(
      align   = "center",
      columns = tidyselect::everything()
    ) |>
    gt::cols_label(
      retention       = "Retention",
      n1              = gt::md("*n*"),
      n2              = gt::md("*n*"),
      p1              = gt::md("*p*"),
      p2              = gt::md("*p*"),
      look1_realistic = gt::md("*Realistic*"),
      look1_worst     = gt::md("*Worst*"),
      look2_power     = "Overall power",
      recruit_look1   = "Look 1",
      recruit_extra   = "Look 2",
      recruit_total   = "Total"
    ) |>
    gt::tab_source_note(gt::md(sprintf(
      paste(
        "Illustration of the pre-registered rule, with %d participants",
        "recruited for look 1. &alpha; = %s is spent at look 1 and the rest",
        "at look 2 (giving &alpha; = .05 overall). *n* refers to the Naive",
        "analysis set sample size and is reported as cumulative at look 2,",
        "calibrated for %s overall power under the *Worst* scenario; the",
        "*p*-value threshold at look 2 refers to a test of all participants of",
        "the Naive set. Recruitment for look 2 assumes the retention of look 1.",
        "Computed by `{rpact}` with *t*-based power."
      ),
      recruit1,
      sub("0.", ".", round(alpha1, 3), fixed = TRUE),
      overall_power
    )))
}


#' The top-up that the pre-registered rule yields at a range of retention rates
#'
#' Applies the rule to each retention rate: look 1 analyses the participants of
#' the first recruitment who reach the Naive set (or recruitment continues until
#' `n1_min` do), and look 2 brings the Naive set to the smallest sample that
#' gives the design the target overall power under `d`. The extra recruitment
#' assumes the same retention as look 1.
#'
#' @param retention Numeric. The retention rates to illustrate, e.g. `0.50`.
#' @param recruit1 Numeric(1). Participants recruited for look 1.
#' @param n1_min Numeric(1). The smallest Naive set analysed at look 1.
#' @param alpha Numeric(1). The overall one-sided significance level.
#' @param alpha1 Numeric(1). The alpha spent at look 1.
#' @param d Numeric(1). The true effect size look 2 is sized for.
#' @param power Numeric(1). The target overall power under `d`.
#' @param scenarios Named numeric. The effect sizes whose look-1 power is
#'   reported, named by scenario.
#'
#' @returns A tibble with one row per retention rate: `retention`, `n1`, `n2`,
#'   the critical values `z1` and `z2`, the nominal one-sided significance
#'   levels `p1` and `p2`, the participants recruited for look 1
#'   (`recruit_look1`), for look 2 (`recruit_extra`) and in total
#'   (`recruit_total`), and the look-1 power under each scenario
#'   (`look1_<scenario>`).
topup_by_retention <- function(
    retention,
    recruit1,
    n1_min,
    alpha,
    alpha1,
    d,
    power,
    scenarios
) {
  purrr::map_dfr(retention, function(r) {
    n1 <- max(n1_min, floor(recruit1 * r))
    recruit1_actual <- max(recruit1, ceiling(n1 / r))
    n2 <- size_topup(n1, alpha, "fixed", alpha1, d = d, power = power)
    design <- design_topup(n1, n2, alpha, "fixed", alpha1)
    look1 <- purrr::map_dbl(scenarios, \(x) power_topup(design, n2, x)[["look1"]])
    extra <- ceiling((n2 - n1) / r)

    tibble::tibble(
      retention     = r,
      n1            = n1,
      n2            = n2,
      z1            = design$criticalValues[1L],
      z2            = design$criticalValues[2L],
      p1            = design$stageLevels[1L],
      p2            = design$stageLevels[2L],
      recruit_look1 = recruit1_actual,
      recruit_extra = extra,
      recruit_total = recruit1_actual + extra,
      !!!purrr::set_names(as.list(look1), paste0("look1_", names(look1)))
    )
  })
}


## ---- 3 · Decision tree ------------------------------------------------------

#' Table of the decision tree
#'
#' @param decisions A tibble with one row per look and combination of test
#'   results: `look` (1 or 2), `bi2` and `bi3` (`"pass"` or `"fail"`),
#'   `replication`, `next_step` and `note`.
#' @param thresholds Character(2). The one-sided significance thresholds of
#'   looks 1 and 2 as printed, e.g. `c("< .045", "< .007")`.
#'
#' @returns A `gt` table grouped by look.
tbl_decisions <- function(decisions, thresholds) {
  decisions |>
    dplyr::mutate(
      look = sprintf(
        "Look %d (one-sided *p* %s)",
        look,
        thresholds[look]
      ),
      next_step = dplyr::if_else(
        is.na(note),
        next_step,
        paste0(next_step, "; ", note)
      ) |>
        stringr::str_replace_all("-03", "`fba-allergy-online-03`") |>
        stringr::str_replace_all("BI([23])", "BI~\\1~"),
      replication = sprintf("**%s**", replication)
    ) |>
    dplyr::select(
      look,
      bi2,
      bi3,
      replication,
      next_step
    ) |>
    gt::gt(
      groupname_col = "look",
      process_md = TRUE
    ) |>
    gt::fmt_markdown(columns = c("replication", "next_step")) |>
    gt::cols_label(
      bi2         = gt::md("BI~2~"),
      bi3         = gt::md("BI~3~"),
      replication = "Replication",
      next_step   = "Next step"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = c("bi2", "bi3")
    ) |>
    gt::tab_source_note(gt::md(paste(
      "*Pass*: the one-sided *t*-test rejects the null hypothesis of no",
      "blocking at the threshold of the look. The replication statement is",
      "decided by BI~2~ analysis alone, BI~3~ analysis results guide only the",
      "decision about index to use in the extension study",
      "`fba-allergy-online-03`."
    )))
}


#' Figure of the decision tree
#'
#' Draws the two looks as nodes, each with one arrow per combination of test
#' results to the decision it leads to. The combination with both indices
#' failing at look 1 leads to look 2.
#'
#' @inheritParams tbl_decisions
#' @param look_labels Character(2). The labels of the look nodes, e.g. with
#'   their sample size and threshold.
#'
#' @returns A `ggplot`.
fig_decision_tree <- function(decisions, look_labels) {
  html <- function(x) {
    x |>
      stringr::str_replace_all("\n", "<br>") |>
      stringr::str_replace_all("BI([23])", "BI<sub>\\1</sub>")
  }

  # Look 1 outcomes in a column at x = 1, look 2 in a column at x = 2, the
  # look-2 node in the look-1 column, level with the look-2 outcomes
  look1 <- dplyr::filter(decisions, look == 1L)
  look2 <- dplyr::filter(decisions, look == 2L)
  stopifnot(nrow(look1) == 4L, nrow(look2) == 4L)
  continues <- look1$next_step == "Look 2"
  stopifnot(sum(continues) == 1L)

  y_look2 <- rev(seq_len(nrow(look2)))
  y_look1 <- numeric(nrow(look1))
  y_look1[continues] <- mean(y_look2)
  y_look1[!continues] <- max(y_look2) + rev(seq_len(sum(!continues))) + 0.5

  outcomes <- dplyr::bind_rows(
    dplyr::mutate(look1, x = 1, y = y_look1),
    dplyr::mutate(look2, x = 2, y = y_look2)
  ) |>
    dplyr::mutate(
      kind = dplyr::case_when(
        next_step == "Look 2" ~ "Look 2",
        replication == "FAILURE" & next_step == "Change course" ~ "Change course",
        TRUE ~ replication
      ),
      label = dplyr::if_else(
        kind == "Look 2",
        html(look_labels[2L]),
        html(paste(replication, next_step, sep = "\n"))
      ),
      edge = html(sprintf("BI2 %s · BI3 %s", bi2, bi3))
    )

  # Boxes of the outcome columns are left-aligned at x, so every arrow can end
  # the same small distance before its box
  gap <- 0.015       # from arrow tip to the left edge of the target box
  root_out <- 0.13   # from the Look 1 node's centre to its right edge
  look2_out <- 0.2   # from the Look 2 node's left edge (x) to its right edge
  
  root <- tibble::tibble(x = 0, y = mean(y_look1), label = html(look_labels[1L]))
  look2_node <- dplyr::filter(outcomes, kind == "Look 2")
  
  # Arrows from the right edge of their source box to just before their target
  # box, and their labels on the line, 60% of the way along
  edges <-
    dplyr::bind_rows(
      dplyr::filter(outcomes, look == 1L) |>
        dplyr::mutate(xs = root$x + root_out, y0 = root$y),
      dplyr::filter(outcomes, look == 2L) |>
        dplyr::mutate(xs = look2_node$x + look2_out, y0 = look2_node$y)
    ) |>
    dplyr::mutate(
      xe = x - gap,
      xl = xs + 0.6 * (xe - xs),
      yl = y0 + 0.6 * (y - y0)
    )
  
  fills <- scales::alpha(
    c(
      "SUCCESS"       = "#009E73",
      "FAILURE"       = "#E69F00",
      "Look 2"        = "#999999",
      "Change course" = "#D55E00"
    ),
    0.35
  )

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data    = edges,
      mapping = ggplot2::aes(x = xs, y = y0, xend = xe, yend = y),
      arrow     = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
      linewidth = 0.6,
      colour    = "grey40"
      
    ) +
    ggtext::geom_richtext(
      data    = edges,
      mapping = ggplot2::aes(x = xl, y = yl, label = edge),
      size          = 2.8,
      fill          = "white",
      label.colour  = NA,
      label.padding = ggplot2::unit(rep(0.1, 4), "lines")
    ) +
    ggtext::geom_richtext(
      data    = root,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size       = 3.2,
      fill       = "grey90",
      lineheight = 0.95
    ) +
    ggtext::geom_richtext(
      data    = outcomes,
      mapping = ggplot2::aes(x = x, y = y, label = label, fill = kind),
      size       = 3,
      lineheight = 0.95,
      hjust      = 0
    ) +
    ggplot2::scale_fill_manual(
      values = fills,
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.25, 2.3)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::theme_void()
}
