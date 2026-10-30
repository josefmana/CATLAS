# 02-feasibility-study.R
#
# Contains all functions bespoken to the `02-feasbility-study.qmd` notebook.
# 
# Function that are more general live either in `../../../common/R` when
# CATLAS-manuscripts specific (e.g., printing functions and visual identity),
# `{catlasr}` R package if CATLAS-project specific but manuscript-agnostic (e.g.,
# data download and preparation, indeces computation, basic visualisation) or
# `{mathpsychr}` R package if CATLAS-agnostic (i.e., anything mathematical
# psychology serving more projects, such as eye-tracking data processing).
#
# The file is split by the notebook sections in a one-to-one manner, with
# subheading of the file mapping to the `R-code` callouts from the text. If any
# function finds its use in several notebook sections, it is listed here under
# the section in which it makes its debut.


# ---- 2 · Participants --------------------------------------------------------

## ---- 2.1 · Attrition funnel -------------------------------------------------

#' Count the participants remaining after each exclusion rule is applied
#'
#' Replays the exclusions recorded in provenance on the imported metadata, in
#' the order they were applied.
#'
#' @param meta Metadata of the bundle as imported, with `id` as text and a
#'   factor variable `channel`.
#' @param exclusions The `exclusions` field of the screened bundle's provenance.
#' @param rule_labels Named character. A short label per rule name; rules
#'   without one keep their original name.
#'
#' @returns A tibble with one row per exclusion step:
#' 
#' * `step` (label),
#' * `removed` (`NA` for the first step, all JATOS records),
#' * one column per channel, and
#' * `Total`.
count_funnel <- function(meta, exclusions, rule_labels) {
  step_ids <- c(
    list(character(0)),
    purrr::map(exclusions, \(e) as.character(e$ids))
  )

  step_labels <- c(
    "JATOS records",
    purrr::map_chr(
      exclusions,
      \(e) dplyr::coalesce(unname(rule_labels[e$rule]), e$rule)
    )
  )

  purrr::imap_dfr(step_ids, function(ids, i) {
    gone <- unlist(step_ids[seq_len(i)])
    meta |>
      dplyr::filter(!id %in% gone) |>
      dplyr::count(channel, .drop = FALSE) |>
      dplyr::mutate(
        step    = i,
        removed = if (i == 1L) NA_integer_ else length(ids)
      )
  }) |>
    tidyr::pivot_wider(
      names_from  = "channel",
      values_from = "n"
    ) |>
    dplyr::mutate(
      Total = as.integer(rowSums(dplyr::pick(dplyr::all_of(levels(meta$channel))))),
      step  = step_labels[step]
    )
}


#' Identifiers excluded by one recorded rule
#'
#' @inheritParams count_funnel
#' @param rule Character(1). The rule name.
#'
#' @returns Character.
ids_excluded_by <- function(exclusions, rule) {
  exclusions |>
    purrr::keep(\(e) e$rule == rule) |>
    purrr::map("ids") |>
    unlist() |>
    as.character()
}


#' Table of the attrition funnel
#'
#' @param funnel [count_funnel()]'s result.
#' @param channels Character. Channel names, in column order.
#'
#' @returns A `gt` table.
tbl_funnel <- function(funnel, channels) {
  funnel |>
    dplyr::select(
      step,
      removed,
      dplyr::all_of(channels),
      Total
    ) |>
    gt::gt() |>
    gt::cols_label(
      step    = "Step",
      removed = "Removed"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"step"
    ) |>
    gt::sub_missing(
      columns      = "removed",
      missing_text = "---"
    ) |>
    gt::tab_spanner(
      label   = "Remaining",
      columns = c(dplyr::all_of(channels), "Total")
    )
}


#' Finds where the runs excluded as incomplete stopped
#'
#' @param progress Result of [catlasr::trace_progress()] on the imported bundle,
#'   with `channel` joined.
#' @param ids Character. Identifiers excluded as incomplete.
#' @param stage_labels Named character. A label per stage name.
#'
#' @returns `progress` filtered to `ids`, with a column `where` in words.
locate_stops <- function(progress, ids, stage_labels) {
  progress |>
    dplyr::filter(id %in% ids) |>
    dplyr::mutate(
      where = dplyr::if_else(
        n_trials == 0L,
        "Before the first learning trial",
        sprintf("%s, block %d", stage_labels[last_stage], last_block)
      )
    )
}


#' Table of where the incomplete runs stopped
#'
#' @param stopped From [locate_stops()].
#'
#' @returns A `gt` table.
tbl_stopped <- function(stopped) {
  stopped |>
    dplyr::select(
      id,
      channel,
      n_trials,
      where
    ) |>
    gt::gt() |>
    gt::cols_align(
      align   = "left",
      columns = "id"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"id"
    ) |>
    gt::cols_label(
      id       = "Participant",
      channel  = "Channel",
      n_trials = "Experiment trials",
      where    = "Last recorded trial"
    )
}


#' Writes a sentence about analysed participants with missing trials
#'
#' @param progress As in [locate_stops()].
#' @param ids Character. Identifiers of the full sample.
#' @param n_test_blocks Integer(1). Number of test blocks in the design.
#'
#' @returns Character(1).
write_partial_runs <- function(progress, ids, n_test_blocks) {
  partial <-
    progress |>
    dplyr::filter(
      id %in% ids,
      !(learning_complete & test_complete)
    )

  if (nrow(partial) == 0L) {
    return("Every participant of the full sample completed all learning stages and the whole test stage.")
  }

  sprintf(
    "In the full sample, %d participant%s analysed despite missing trials: %s.",
    nrow(partial),
    if (nrow(partial) == 1L) " is" else "s are",
    paste(
      sprintf(
        "%s (%s, %d of %d test blocks)",
        partial$id,
        dplyr::if_else(partial$learning_complete, "learning complete", "learning incomplete"),
        partial$n_test_blocks,
        n_test_blocks
      ),
      collapse = "; "
    )
  )
}


#' Describe the sample of a bundle
#'
#' @param x A `catlas_rawdata` bundle.
#'
#' @returns A tibble with columns `characteristic` and `value` (text).
describe_sample <- function(x) {
  m <- catlasr::extract_meta(x)
  sex <- table(tidyr::replace_na(as.character(m$sex), "not reported"))

  tibble::tibble(
    variable = c(
      "Sample size",
      rep("Age (years)", 2),
      rep("Sex", length(sex)),
      rep("Prior knowledge", 2)
    ),
    characteristic = c(
      "N",
      "mean (SD)",
      "range",
      names(sex),
      "yes",
      "no"
    ),
    value = c(
      as.character(nrow(m)),
      sprintf("%.1f (%.1f)", mean(m$age, na.rm = TRUE), stats::sd(m$age, na.rm = TRUE)),
      sprintf("%.0f\u2013%.0f", min(m$age, na.rm = TRUE), max(m$age, na.rm = TRUE)),
      as.character(as.integer(sex)),
      as.character(sum(m$prior_knowledge %in% TRUE)),
      as.character(sum(m$prior_knowledge %in% FALSE))
    )
  )
}


#' Table of sample characteristics, full sample against primary set
#'
#' @param full,primary Data bundles of the full sample and of the primary set.
#'
#' @returns A `gt` table.
tbl_sample <- function(full, primary) {
  dplyr::full_join(
    describe_sample(full) |> dplyr::rename(full = value),
    describe_sample(primary) |> dplyr::rename(primary = value),
    by = c("variable", "characteristic")
  ) |>
    dplyr::mutate(dplyr::across(
      .cols = c(full, primary),
      .fns  = \(v) tidyr::replace_na(v, "0")
    )) |>
    gt::gt(groupname_col = "variable") |>
    gt::cols_align(
      align   = "center",
      columns = -"characteristic"
    ) |>
    gt::cols_label(
      characteristic = "",
      full           = "Full sample",
      primary        = "Primary set"
    ) |>
    gt::tab_style(
      style     = gt::cell_text(indent = gt::px(20)),
      locations = gt::cells_body(columns = "characteristic")
    )
}


#' Timed-out learning trials per participant
#'
#' @param scores From [catlasr::score_learning()].
#'
#' @returns A tibble: `id`, `n_timed_out`, `n_trials`.
count_timeouts <- function(scores) {
  scores |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      n_timed_out = sum(n_timed_out),
      n_trials = sum(n_trials),
      .groups = "drop"
    )
}


#' Figure of the time on task
#'
#' @param times Rows of [catlasr::trace_progress()] for the full sample, with
#'   an additional column `set` ("Primary set" or "Excluded on learning").
#' @param stage_labels Named character. A label per stage name.
#'
#' @returns A `ggplot`.
fig_time <- function(times, stage_labels) {
  stage_cols <- paste0("task_min_", names(stage_labels))

  times |>
    dplyr::select(
      id,
      set,
      duration_min,
      task_min,
      dplyr::all_of(stage_cols)
    ) |>
    tidyr::pivot_longer(
      -c(id, set),
      names_to  = "measure",
      values_to = "minutes"
    ) |>
    dplyr::filter(!is.na(minutes)) |>
    dplyr::mutate(
      measure = factor(
        measure,
        levels = c("duration_min", "task_min", stage_cols),
        labels = c("Whole run", "Responding,\nall stages", paste0("Responding,\n", stage_labels))
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = measure,
      y = minutes
    ) +
    ggplot2::geom_boxplot(
      outlier.shape = NA,
      width         = 0.5,
      colour        = "grey40"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = set),
      position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1),
      size     = 2.5,
      alpha    = 0.6
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Primary set"          = "#0072B2",
        "Excluded on learning" = "#D55E00"
      ),
      breaks = c("Primary set", "Excluded on learning"),
      name   = NULL
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(
      x = NULL,
      y = "Minutes"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position        = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification   = c(1, 1),
      legend.background      = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major.x = ggplot2::element_blank()
    )
}


## ---- 2.2 · Learning check ---------------------------------------------------

#' Learning cut-offs on complete data
#'
#' The smallest number of correct responses that passes the one-sided exact
#' binomial test of [catlasr::test_learning()] against 0.5.
#'
#' @param alpha Numeric(1). Significance level.
#' @param n Integer. Trials per stage.
#'
#' @returns Integer, as many elements as `n`.
compute_cutoffs <- function(alpha, n) {
  stats::qbinom(1 - alpha, n, 0.5) + 1
}


#' Participants above chance per learning stage
#'
#' @param tests From [catlasr::test_learning()].
#'
#' @returns A tibble: `stage`, `passed`, `n`.
count_stage_passes <- function(tests) {
  tests |>
    dplyr::group_by(stage) |>
    dplyr::summarise(
      passed  = sum(above_chance %in% TRUE),
      n       = dplyr::n(),
      .groups = "drop"
    )
}


#' Figure of learning across stages
#' 
#' \{catlasr\} functions are deliberately garrulous to warn about all the
#' pitfalls coming with them. The notebook is not the place for such a chat, so
#' we suppress it.
#' 
#' @param x A `catlas_rawdata` object.
#' 
#' @returns A `ggplot`
fig_learning <- function(x) {
  suppressMessages(catlasr::plot_learning(catlasr::score_learning(x)))
}


#' Figure of every participant's learning curves, failed participants in grey
#'
#' Participants excluded on learning are drawn in greys: dark for blocking cues
#' and critical compounds, light for control cues and fillers. The colours of
#' the other participants copy those of [catlasr::plot_learning()].
#'
#' @param scores From [catlasr::score_learning()].
#' @param ids_failed Character. Identifiers excluded on learning.
#'
#' @returns A `ggplot`.
fig_learning_participants <- function(scores, ids_failed) {
  learning_colours <- c(
    blocking        = "#D55E00",
    control         = "#0072B2",
    legit           = "#CC79A7",
    filler          = "#009E73",
    blocking_failed = "grey25",
    control_failed  = "grey65",
    legit_failed    = "grey25",
    filler_failed   = "grey65"
  )
  
  suppressMessages(
    scores |>
      dplyr::mutate(type = dplyr::if_else(id %in% ids_failed, paste0(type, "_failed"), type)) |>
      catlasr::plot_learning(level = "participant") +
      ggplot2::scale_colour_manual(
        values = learning_colours,
        breaks = c("blocking", "control", "legit", "filler"),
        name   = NULL
      )
  )
}


#' Table of the per-stage learning tests
#'
#' @param passes From [count_stage_passes()].
#' @param cutoffs,trials Integer. Cut-offs and trials per stage, in the order
#'   of `passes$stage`.
#' @param stage_labels Named character. A label per stage name.
#'
#' @returns A `gt` table.
tbl_stage_tests <- function(passes, cutoffs, trials, stage_labels) {
  passes |>
    dplyr::mutate(
      stage   = stage_labels[stage],
      cut_off = sprintf("%d of %d (%.1f%%)", cutoffs, trials, 100 * cutoffs / trials),
      passed  = sprintf("%d of %d", passed, n)
    ) |>
    dplyr::select(
      stage,
      cut_off,
      passed
    ) |>
    gt::gt() |>
    gt::cols_align(
      align   = "center",
      columns = -"stage"
    ) |>
    gt::cols_label(
      stage   = "Stage",
      cut_off = "Cut-off on complete data",
      passed  = gt::md("*N* passed")
    )
}


#' Table of the participants excluded due to failing the learning check
#'
#' @param excluded Rows of [catlasr::screen_learning()] with `exclude`, with
#'   `channel` joined.
#'
#' @returns A `gt` table.
tbl_learning_excluded <- function(excluded) {
  excluded |>
    dplyr::select(
      id,
      channel,
      reason
    ) |>
    gt::gt() |>
    gt::cols_label(
      id      = "Participant",
      channel = "Channel",
      reason  = "Failed tests"
    )
}


#' Screen the learning tests at several levels of alpha
#'
#' @param scores From [catlasr::score_learning()].
#' @param alphas Numeric. Levels of alpha.
#' @param n Integer. Trials per learning stage.
#' @param ids_labels Character. Identifiers recruited through LABELS.
#'
#' @returns A tibble with one row per alpha: the cut-off per stage, and the
#'   participants kept, overall and from LABELS.
screen_alphas <- function(scores, alphas, n, ids_labels) {
  purrr::map_dfr(alphas, function(a) {
    screen <-
      catlasr::test_learning(scores, alpha = a, silence = TRUE) |>
      catlasr::screen_learning(rule = "any", silence = TRUE)
    kept <- screen$id[!screen$exclude]
    k <- compute_cutoffs(a, n)

    tibble::tibble(
      alpha       = a,
      stage1      = sprintf("%d/%d", k[1], n[1]),
      stage2      = sprintf("%d/%d", k[2], n[2]),
      stage3      = sprintf("%d/%d", k[3], n[3]),
      kept_all    = print_n_pct(length(kept), nrow(screen)),
      kept_labels = print_n_pct(sum(kept %in% ids_labels), sum(screen$id %in% ids_labels))
    )
  })
}


#' Table of the learning screen across alpha
#'
#' @param sensitivity From [screen_alphas()].
#' @param applied Numeric(1). The level of alpha applied, set in bold.
#'
#' @returns A `gt` table.
tbl_alpha <- function(sensitivity, applied) {
  sensitivity |>
    dplyr::mutate(alpha = sub("0.", ".", sprintf("%.3f", alpha), fixed = TRUE)) |>
    gt::gt() |>
    gt::cols_align(
      align   = "left",
      columns = "alpha"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"alpha"
    ) |>
    gt::cols_label(
      alpha       = "\u03b1",
      stage1      = "Stage 1",
      stage2      = "Stage 2",
      stage3      = "Stage 3",
      kept_all    = "Full sample",
      kept_labels = "LABELS"
    ) |>
    gt::tab_spanner(
      label   = "Cut-off",
      columns = c("stage1", "stage2", "stage3")
    ) |>
    gt::tab_spanner(
      label   = "Included",
      columns = tidyselect::starts_with("kept")
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = alpha == sub("0.", ".", sprintf("%.3f", applied)))
    )
}


#' Table of the analysis sets
#'
#' @param meta_screened Metadata of the primary set.
#' @param ids_structural Character. Identifiers of the full sample.
#'
#' @returns A `gt` table.
tbl_analysis_sets <- function(meta_screened, ids_structural) {
  tibble::tibble(
    set    = c("1 \u00b7 Primary", "2 \u00b7 Full sample", "3 \u00b7 Primary, no prior knowledge"),
    data   = c("After all exclusions", "Before the learning-based exclusion", "After all exclusions"),
    filter = c("None", "None", "Prior knowledge not reported"),
    n      = c(
      nrow(meta_screened),
      length(ids_structural),
      sum(!(meta_screened$prior_knowledge %in% TRUE))
    )
  ) |>
    gt::gt() |>
    gt::cols_label(
      set    = "Set",
      data   = "Data",
      filter = "Filter",
      n      = "N"
    )
}


#' The analysis sets as participant identifiers
#'
#' @inheritParams tbl_analysis_sets
#'
#' @returns A named list of character vectors, one per analysis set, in the
#'   order of [tbl_analysis_sets()].
list_analysis_sets <- function(meta_screened, ids_structural) {
  ids_screened <- as.character(meta_screened$id)

  list(
    "1 \u00b7 Primary" = ids_screened,
    "2 \u00b7 Full sample" = ids_structural,
    "3 \u00b7 Primary, no prior knowledge" = ids_screened[!(meta_screened$prior_knowledge %in% TRUE)]
  )
}


#' The recorded exclusion rules as a numbered Markdown list
#'
#' @inheritParams count_funnel
#'
#' @returns Character, one list item per rule, for a chunk with
#'   `results: asis`.
write_rules <- function(exclusions, rule_labels) {
  purrr::imap_chr(exclusions, function(e, i) {
    sprintf(
      "%d. **%s.** %s *Pilot: removed %d.*",
      i,
      dplyr::coalesce(unname(rule_labels[e$rule]), e$rule),
      e$description,
      e$n
    )
  })
}


# ---- 3 · Blocking ------------------------------------------------------------

## ---- 3.1 · Blocking calculations --------------------------------------------

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


#' Estimate blocking indices in every analysis set
#'
#' Aggregates per-participant indices within each set and adds a one-sample
#' t-test against zero (i.e., no blocking), its confidence interval, and the
#' standardised mean (Cohen's \eqn{d_z} = mean / SD) with its exact confidence
#' interval (also or two-sided and one-sided tests, respectively). With
#' `alternative = "greater"`, the test is one-sided in the direction of
#' blocking, and 90% CIs are reported; otherwise the test is two-sided
#' and 95% CIs. are reported..
#'
#' @param indices Per-participant indices from [catlasr::index_blocking()],
#'   possibly several selections of blocks bound by rows.
#' @param sets From [list_analysis_sets()].
#' @param alternative Character(1). `"two.sided"` (default) or `"greater"`.
#'
#' @returns A tibble with one row per data set, index and selection of blocks:
#'   `set`, `index`, `blocks`, `n`, `estimate`, `sd`, `se`, `ci_low`,
#'   `ci_high`, `t`, `df`, `p`, `alternative`, `d`, `d_low`, `d_high`, `perc`.
estimate_sets <- function(
    indices,
    sets,
    alternative = c("two.sided", "greater")
) {
  alternative <- match.arg(alternative)
  alpha <- if (alternative == "greater") .10 else .05
  
  purrr::imap_dfr(sets, function(ids, set) {
    indices |>
      dplyr::filter(id %in% ids) |>
      catlasr::aggregate_blocking(silence = TRUE) |>
      dplyr::mutate(set = set, .before = 1)
  }) |>
    dplyr::mutate(set = factor(set, levels = names(sets))) |>
    tidyr::complete(
      set,
      tidyr::nesting(index, blocks),
      fill = list(n_participants = 0L)
    ) |>
    dplyr::mutate(
      df = n_participants - 1L,
      t = estimate / se,
      p = if (alternative == "greater") {
        stats::pt(t, pmax(df, 1L), lower.tail = FALSE)
      } else {
        2 * stats::pt(-abs(t), pmax(df, 1L))
      },
      half = stats::qt(1 - alpha / 2, pmax(df, 1L)) * se,
      ci_low = estimate - half,
      ci_high = estimate + half,
      d = estimate / sd,
      d_ci = purrr::map2(t, n_participants, \(t, n) ci_d_one_sample(t, n, 1 - alpha)),
      d_low = purrr::map_dbl(d_ci, 1),
      d_high = purrr::map_dbl(d_ci, 2),
      alternative = alternative,
      perc = 1 - alpha
    ) |>
    dplyr::select(
      set,
      index,
      blocks,
      n = n_participants,
      estimate, sd,
      se, ci_low, ci_high, perc,
      t, df, p, alternative,
      d, d_low, d_high
    )
}


#' Table of a blocking index across the analysis sets
#'
#' @param estimates From [estimate_sets()].
#' @param which_index Character(1). The index by its \{catlasr\} column name,
#'   `"bi_confidence"` (BI2) or `"bi_choice"` (BI3).
#' @param which_blocks Character(1). The selection of blocks, as labelled by
#'   [catlasr::index_blocking()], e.g. `"1"`.
#'
#' @returns A `gt` table. When the test is one-sided, the note column says so
#'   and the two-sided CI is 90% instead of 95% so that its lower bound aligns
#'   with a one-sided 95% interval.
tbl_blocking <- function(estimates, which_index, which_blocks) {
  one_sided <- identical(unique(estimates$alternative), "greater")
  
  note <- sprintf(
    paste(
      "N: number of participants; SD: standard deviation; SE: standard error",
      "of the mean; CI: confidence interval; t(df): t-statistic with",
      "associated degrees of freedom; %s p-value; d~z~: Cohen's d, mean divided",
      "by the SD"
    ),
    if (one_sided) "one-sided" else "two-sided"
  )
  
  estimates |>
    dplyr::filter(
      index == which_index,
      blocks == which_blocks
    ) |>
    dplyr::mutate(
      ci = dplyr::if_else(is.na(ci_low), NA_character_, sprintf("[%.2f, %.2f]", ci_low, ci_high)),
      t = dplyr::if_else(is.na(t), NA_character_, sprintf("%.2f (%d)", t, df)),
      p = dplyr::case_when(
        is.na(p) ~ NA_character_,
        p < 0.001 ~ "< .001",
        TRUE ~ sub("^0", "", sprintf("%.3f", p))
      ),
      d_ci = dplyr::if_else(is.na(d_low), NA_character_, sprintf("[%.2f, %.2f]", d_low, d_high))
    ) |>
    dplyr::select(
      set, n,
      estimate, sd, se, ci,
      t, p,
      d, d_ci
    ) |>
    gt::gt() |>
    gt::fmt_number(
      columns  = c("estimate", "sd", "se", "d"),
      decimals = 2
    ) |>
    gt::sub_missing(missing_text = "\u2013") |>
    gt::tab_style(
      style     = gt::cell_text(whitespace = "nowrap"),
      locations = list(gt::cells_column_labels(), gt::cells_body(columns = c("ci", "t")))
    ) |>
    gt::tab_spanner(
      label = "Estimate",
      columns = c("estimate", "sd", "se", "ci")
    ) |>
    gt::tab_spanner(
      label = "Stat. test",
      columns = c("t", "p")
    ) |>
    gt::tab_spanner(
      label = "Effect size",
      columns = c("d", "d_ci")
    ) |>
    gt::cols_align(
      align   = "left",
      columns = "set"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"set"
    ) |>
    gt::cols_label(
      set      = "Set",
      n        = "N",
      estimate = "Mean",
      sd       = "SD",
      se       = "SE",
      ci       = sprintf("%s%% CI", if (one_sided) "90" else "95"),
      t        = "t (df)",
      p        = "p",
      d        = gt::html("<i>d</i><sub>z</sub>"),
      d_ci     = sprintf("%s%% CI", if (one_sided) "90" else "95")
    ) |>
    gt::tab_options(table.font.size = gt::pct(90)) |>
    gt::tab_source_note(gt::md(note)) |>
    gt::tab_footnote(
      footnote = paste(
        "Computed via inverting the noncentral t distribution of the observed",
        "t-statistic."
      ),
      locations = gt::cells_column_labels(d_ci)
    )
}


#' Table of the estimates for the power analysis
#'
#' Effect size estimates (standardised mean, i.e., Cohen's \eqn{d_z}) with
#' their 95% or 90% CIs (for two-tailed and one-tailed test, respectively)
#' across provided analysis sets for BI2 and BI3.
#'
#' @param estimates From [estimate_sets()].
#'
#' @returns A `gt` table.
tbl_power <- function(estimates) {
  naming <-
    estimates |>
    dplyr::select(index, blocks) |>
    dplyr::distinct() |>
    dplyr::mutate(
      name = sprintf(
        "%s (Block%s %s)",
        dplyr::if_else(index == "bi_confidence", "BI~2~", "BI~3~"),
        dplyr::if_else(blocks == "1", "", "s"),
        blocks
      )
    )
  
  tab <- 
    estimates |>
    dplyr::mutate(ci = dplyr::if_else(
      is.na(d_low),
      NA_character_,
      sprintf("[%.2f, %.2f]", d_low, d_high)
    )) |>
    dplyr::left_join(
      naming,
      by = c("index", "blocks")
    ) |>
    dplyr::select(
      set,
      name,
      d,
      ci
    ) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = c(d, ci)
    ) |>
    dplyr::select(
      set,
      tidyselect::contains("BI~2~"),
      tidyselect::contains("BI~3~")
    ) |>
    gt::gt() |>
    gt::fmt_number(
      columns  = tidyselect::starts_with("d_"),
      decimals = 2
    ) |>
    gt::cols_align(
      align   = "left",
      columns = "set"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"set"
    ) |>
    gt::cols_label(
      set = "Set",
      tidyselect::starts_with("d_")  ~ gt::md("*d*~z~"),
      tidyselect::starts_with("ci_") ~ sprintf("%s%% CI", if (identical(unique(estimates$alternative), "greater")) "90" else "95")
    ) |>
    gt::tab_source_note(gt::md(
      paste(
        "*d*~z~: Cohen's d, mean divided by the standard deviation; CI:",
        "confidence interval computed via inverting the noncentral t",
        "distribution of the observed t-statistic."
      )
    ))
  
  for (i in seq_len(nrow(naming))) {
    tab <- tab |>
      gt::tab_spanner(
        label   = gt::md(naming$name[[i]]),
        columns = tidyselect::ends_with(naming$name[[i]])
      )
  }
  
  tab
}


#' Figure of BI2 and BI3 per test block and pooled
#'
#' @param per_block,pooled Per-participant indices from
#'   [catlasr::index_blocking()], per block and over all blocks.
#' @param ids Character. Participants to draw.
#'
#' @returns A `ggplot`.
fig_blocking_blocks <- function(per_block, pooled, ids) {
  suppressMessages(
    dplyr::bind_rows(per_block, pooled) |>
      dplyr::filter(id %in% ids) |>
      catlasr::plot_blocking(
        labels = c(
          bi_confidence = 'BI[2]*":"~"confidence-weighted choice score"',
          bi_choice     = 'BI[3]*":"~"choice-proportion score"'
        ),
        parse = TRUE
      )
  )
}


#' Correlations of selected index between test blocks, per analysis set
#'
#' @param per_block Per-participant indices per block, from
#'   [catlasr::index_blocking()] with `by_block = TRUE`.
#' @param sets Named list of participant identifiers, all elements of
#'   [list_analysis_sets()].
#' @param which_bi Character(1). Either `"bi_confidence"` for BI2 or
#'   `"bi_choice"` for BI3, saying which blocking index to use in computations.
#'
#' @returns A tibble with one row per set and pair of blocks of the lower
#'   triangle: `set`, `block_x`, `block_y`, `n`, `r` (`NA` when a block shows
#'   no variation or fewer than three participants enter).
correlate_blocks <- function(per_block, sets, which_bi) {
  purrr::imap_dfr(sets, function(ids, set) {
    wide <-
      per_block |>
      dplyr::filter(id %in% ids) |>
      dplyr::select(
        id,
        blocks,
        index = tidyselect::all_of(which_bi)
      ) |>
      tidyr::pivot_wider(
        names_from = blocks,
        values_from = index
      )
    
    labels <- setdiff(names(wide), "id")
    
    tidyr::expand_grid(block_x = labels, block_y = labels) |>
      dplyr::filter(match(block_x, labels) < match(block_y, labels)) |>
      dplyr::mutate(
        set = set,
        n = purrr::map2_int(block_x, block_y, \(a, b) sum(!is.na(wide[[a]]) & !is.na(wide[[b]]))),
        r = purrr::map2_dbl(block_x, block_y, function(a, b) {
          ok <- !is.na(wide[[a]]) & !is.na(wide[[b]])
          x <- wide[[a]][ok]
          y <- wide[[b]][ok]
          if (length(x) < 3L || stats::sd(x) == 0 || stats::sd(y) == 0) return(NA_real_)
          stats::cor(x, y)
        }),
        block_x = factor(block_x, levels = labels),
        block_y = factor(block_y, levels = rev(labels))
      )
  }) |>
    dplyr::mutate(set = factor(set, levels = names(sets))) |>
    dplyr::relocate(set)
}


#' Figure of the correlations of selected index between test blocks, one panel
#' per analysis set
#'
#' @param correlations From [correlate_blocks()].
#'
#' @returns A `ggplot`.
fig_block_correlations <- function(correlations) {
  ggplot2::ggplot(correlations) +
    ggplot2::aes(
      x = block_x,
      y = block_y,
      fill = r
    ) +
    ggplot2::geom_tile(
      colour    = "white",
      linewidth = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = dplyr::if_else(is.na(r), "\u2013", sub("^(-?)0", "\\1", sprintf("%.2f", r)))),
      size = 3.4
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "#0072B2",
      mid      = "white",
      high     = "#D55E00",
      midpoint = 0,
      limits   = c(-1, 1),
      na.value = "grey92",
      name     = "r" 
    ) +
    ggplot2::facet_wrap(ggplot2::vars(set)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Test block",
      y = "Test block"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


## ---- 3.2 · Mixed models -----------------------------------------------------

# Model names for fill-in if non-convergence happens:
.stability_models <- c("intercept", "drift", "drift_individual")


#' Trial-level data for the stability models
#'
#' @param scores Per-trial blocking scores, from [catlasr::score_blocking()].
#' @param ids Character. Participants that enter.
#'
#' @returns A tibble with one row per answered test trial: `id`, `pair` (the
#'   test pair to later serve as GLMM's item variable), `block` (1 to 6),
#'   `block0` (`block - 1`, so that intercepts refer to the first test block),
#'   `signed_confidence` (the source of BI2) and `consistent` (0 or 1, the
#'   source of BI3).
prepare_stability_data <- function(scores, ids) {
  scores |>
    dplyr::filter(
      id %in% ids,
      !is.na(consistent)
    ) |>
    dplyr::mutate(
      id = factor(id),
      pair = factor(paste0(cue1, cue2)),
      block = block,
      block0 = block - 1L,
      signed_confidence = signed_confidence,
      consistent = as.integer(consistent),
      .keep = "none"
    )
}


#' Fit the stability models of one blocking index
#'
#' Fits three nested mixed-effects models of the trial-level source of one
#' index, with by-participant and by-test-pair crossed random
#' intercepts:
#'
#' * `intercept`: No drift across test blocks; its intercept is the
#'   model-based version of the blocking index;
#' * `drift`: a common linear change across blocks (`block0` column), its
#'   intercept refers to the first test block;
#' * `drift_individual`: as `drift` plus participant-specific slopes. If its
#'   fit is singular, the slope-intercept correlation is dropped; if that is
#'   singular too, the model is left out and the comparison stops at `drift`.
#'
#' `signed_confidence` (the source of BI2) is modelled as Gaussian with
#' `lme4::lmer()`, fitted by maximum likelihood so that the models can be
#' compared. It is bimodal (no zero), so treat the Gaussian as an approximation.
#' `consistent` (the source of BI3) is modelled as binomial (logit link) with
#' `lme4::glmer()`.
#'
#' @param data From [prepare_stability_data()].
#' @param outcome Character(1). `"signed_confidence"` or `"consistent"`.
#'
#' @returns A named list of fitted models, with attributes `outcome` and
#'   `notes` (a named character vector of the warnings and singular fits met
#'   on the way, by model).
fit_stability_models <- function(
    data,
    outcome = c("signed_confidence", "consistent")
) {
  outcome <- match.arg(outcome)
  
  formulas <- c(
    intercept               = "~ 1 + (1 | id) + (1 | pair)",
    drift                   = "~ block0 + (1 | id) + (1 | pair)",
    drift_individual        = "~ block0 + (1 + block0 | id) + (1 | pair)",
    drift_individual_uncorr = "~ block0 + (1 + block0 || id) + (1 | pair)"
  )
  
  notes <- character(0)
  
  fit <- function(name) {
    formula <- stats::as.formula(paste(outcome, formulas[[name]]))
    caught <- character(0)
    model <- withCallingHandlers(
      if (outcome == "consistent") {
        lme4::glmer(
          formula,
          data    = data,
          family  = stats::binomial,
          control = lme4::glmerControl(optimizer = "bobyqa")
        )
      } else {
        lme4::lmer(
          formula,
          data    = data,
          REML    = FALSE,
          control = lme4::lmerControl(optimizer = "bobyqa")
        )
      },
      warning = function(w) {
        caught <<- c(caught, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        caught <<- c(caught, trimws(conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )
    if (lme4::isSingular(model)) {
      caught <- c(caught, "singular fit")
    }
    if (length(caught) > 0L) {
      notes[[name]] <<- paste(unique(caught), collapse = "; ")
    }
    model
  }
  
  fits <- list(
    intercept = fit("intercept"),
    drift     = fit("drift")
  )
  
  individual <- fit("drift_individual")
  
  if (lme4::isSingular(individual)) {
    individual <- fit("drift_individual_uncorr")
    if (!lme4::isSingular(individual)) {
      fits$drift_individual <- individual
      notes[["drift_individual"]] <- "correlated slope singular; slope-intercept correlation dropped"
    } else {
      notes[["drift_individual"]] <- "random slope singular with and without correlation; model left out"
    }
  } else {
    fits$drift_individual <- individual
  }
  
  structure(fits, outcome = outcome, notes = notes)
}


#' Compare the nested stability models
#'
#' Likelihood-ratio tests of each model against the previous one. Testing a
#' variance at zero lies on the boundary of its parameter space, so the test of
#' `drift_individual` against `drift` is conservative.
#'
#' @param fits From [fit_stability_models()].
#'
#' @returns A tibble with one row per model: `model`, `n_par`, `log_lik`,
#'   `chisq`, `df`, `p` (against the previous model), `aic`, `aic_wt`, `bic`,
#'   `bic_wt`, `r2_conditional`, and `r2_marginal`.
compare_stability_models <- function(fits) {
  perf <-
    suppressMessages(performance::compare_performance(fits)) |> # Says that some models vary only by random effects, which we know
    dplyr::select(
      model   = Name,
      aic     = AIC,
      aic_wt  = AIC_wt,
      bic     = BIC,
      bic_wt  = BIC_wt,
      r2_conditional = R2_conditional,
      r2_marginal    = R2_marginal
    )
  
  comps <- tibble::tibble(
    model   = names(fits),
    n_par   = vapply(fits, \(m) attr(stats::logLik(m), "df"), numeric(1)),
    log_lik = vapply(fits, \(m) as.numeric(stats::logLik(m)), numeric(1))
  ) |>
    dplyr::mutate(
      chisq = 2 * (log_lik - dplyr::lag(log_lik)),
      df    = n_par - dplyr::lag(n_par),
      p     = stats::pchisq(chisq, df, lower.tail = FALSE)
    )
  
  tibble::tibble(model = .stability_models) |>
    dplyr::left_join(comps, by = "model") |>
    dplyr::left_join(perf, by = "model")
}


#' The choice-proportion index implied by a logit-scale prediction
#'
#' Averages the predicted probability of a blocking-consistent choice over the
#' participants and test pairs, i.e., over a normal distribution of the random
#' intercepts of `model` (participants and test pairs combined), and turns it into
#' the scale of BI3: \eqn{n_{pairs} \times (\bar{P} - 0.5)}. This marginal
#' value, not the prediction for a typical participant, is what the aggregate
#' BI3 estimates.
#'
#' Only the random intercepts enter, so `eta` must refer to the first test
#' block (`block0 = 0`) when `model` has participant-specific slopes; any
#' block is fine for models without them.
#'
#' @param model A binomial model from [fit_stability_models()].
#' @param eta Numeric. Predictions on the logit scale.
#'
#' @returns Numeric, as long as `eta`.
marginal_choice_index <- function(model, eta) {
  vc <- as.data.frame(lme4::VarCorr(model))
  intercepts <- vc$grp %in% c("id", "pair") & vc$var1 == "(Intercept)" & is.na(vc$var2)
  sd <- sqrt(sum(vc$vcov[intercepts]))
  n_pairs <- nlevels(model@frame$pair)
  
  vapply(eta, function(x) {
    p <- stats::integrate(
      function(u) stats::plogis(x + u) * stats::dnorm(u, sd = sd),
      lower = -Inf,
      upper = Inf
    )$value
    n_pairs * (p - 0.5)
  }, numeric(1))
}


#' Fixed effects and random-effect SDs of the stability models
#'
#' Wald `100 * (1 - alpha)`% intervals throughout. The intercepts are also given
#' on the scale of the index: for BI2 the scale of the model; for BI3, the
#' marginal value o [marginal_choice_index()], with the interval transformed
#' from that of the intercept (holding the random-effect SDs at their
#' estimates).
#'
#' @param fits From [fit_stability_models()].
#' @param alpha Numeric(1). Significance level, `0.10` by default to reflect
#'   the one-sided main analysis.
#'
#' @returns A tibble with one row per model and parameter: `model`,
#'   `component` (`"fixed"` or `"sd"`), `term`, `estimate`, `se`, `ci_low`,
#'   `ci_high`, `index_scale`, `index_low`, `index_high`.
summarise_stability <- function(fits, alpha = 0.10) {
  outcome <- attr(fits, "outcome")
  
  purrr::imap(fits, function(model, name) {
    beta <- lme4::fixef(model)
    se <- sqrt(diag(as.matrix(stats::vcov(model))))
    
    to_index <- function(x) {
      if (outcome == "consistent") marginal_choice_index(model, x) else x
    }
    
    fixed <- tibble::tibble(
      model     = name,
      component = "fixed",
      term      = names(beta),
      estimate  = unname(beta),
      se        = unname(se),
      ci_low    = estimate - stats::qnorm(1 - alpha / 2) * se,
      ci_high   = estimate + stats::qnorm(1 - alpha / 2) * se
    ) |>
      dplyr::mutate(
        index_scale = dplyr::if_else(term == "(Intercept)", to_index(estimate), NA_real_),
        index_low   = dplyr::if_else(term == "(Intercept)", to_index(ci_low), NA_real_),
        index_high  = dplyr::if_else(term == "(Intercept)", to_index(ci_high), NA_real_)
      )
    
    random <-
      as.data.frame(lme4::VarCorr(model)) |>
      dplyr::filter(is.na(var2)) |>
      dplyr::mutate(
        model = name,
        component = "sd",
        term = dplyr::if_else(
          grp == "Residual",
          "Residual",
          paste0(sub("\\.\\d+$", "", grp), ": ", var1)
        ),
        estimate = sdcor,
        .keep = "none"
      )
    
    dplyr::bind_rows(fixed, random)
  }) |>
    purrr::list_rbind() |>
    tidyr::complete(model = .stability_models, tidyr::nesting(component, term)) |>
    dplyr::arrange(match(model, .stability_models))
}


#' Fit, compare and summarise the stability models of both indices
#'
#' @param scores Per-trial blocking scores, from [catlasr::score_blocking()].
#' @param sets Named list of participant identifiers, e.g. the results of
#'   [list_analysis_sets()].
#'
#' @returns A list with `comparison` and `estimates` (tibbles from
#'   [compare_stability_models()] and [summarise_stability()] with `set` and
#'   `index` added) and `fits` (the models, by set and index).
model_stability <- function(scores, sets) {
  outcomes <- c(bi_confidence = "signed_confidence", bi_choice = "consistent")
  
  fits <- purrr::map(sets, function(ids) {
    data <- prepare_stability_data(scores, ids)
    purrr::map(outcomes, \(outcome) fit_stability_models(data, outcome))
  })
  
  collect <- function(f) {
    purrr::imap_dfr(fits, function(by_index, set) {
      purrr::imap_dfr(by_index, function(models, index) {
        dplyr::mutate(f(models), set = set, index = index, .before = 1)
      })
    }) |>
      dplyr::mutate(set = factor(set, levels = names(sets)))
  }
  
  list(
    comparison = collect(compare_stability_models),
    estimates  = collect(summarise_stability),
    fits       = fits
  )
}


#' Table of the stability model comparison
#'
#' @param comparison The `comparison` element of [model_stability()].
#'
#' @returns A `gt` table.
tbl_stability_comparison <- function(comparison) {
  comparison |>
    dplyr::mutate(
      group = paste0(
        set, ", ",
        dplyr::if_else(index == "bi_confidence", "BI\u2082 (signed confidence, Gaussian)", "BI\u2083 (consistent choice, binomial)")
      ),
      model = dplyr::recode(
        model,
        intercept        = "No drift",
        drift            = "Common drift",
        drift_individual = "Individual drift"
      ),
      lrt = dplyr::if_else(
        is.na(chisq),
        NA_character_,
        sprintf("%.2f (%d)", chisq, as.integer(df))
      ),
      p = dplyr::case_when(
        is.na(p) ~ NA_character_,
        p < 0.001 ~ "< .001",
        TRUE ~ sub("^0", "", sprintf("%.3f", p))
      ),
      aic = dplyr::if_else(
        is.na(aic),
        NA_character_,
        sprintf("%s (%.2f)", format(round(aic, 1), big.mark = ","), round(aic_wt, 2))
      ),
      bic = dplyr::if_else(
        is.na(bic),
        NA_character_,
        sprintf("%s (%.2f)", format(round(bic, 1), big.mark = ","), round(bic_wt, 2))
      )
    ) |>
    dplyr::select(
      group, model,
      n_par, log_lik,
      lrt, p,
      aic, bic,
      r2_conditional, r2_marginal
    ) |>
    gt::gt(groupname_col = "group") |>
    gt::fmt_number(
      columns  = "log_lik",
      decimals = 1
    ) |>
    gt::fmt_number(
      columns  = c("r2_conditional", "r2_marginal"),
      decimals = 3
    ) |>
    gt::tab_spanner(
      label   = "Statistical test",
      columns = c("log_lik", "lrt", "p")
    ) |>
    gt::tab_spanner(
      label   = "Information criteria",
      columns = c("aic", "bic")
    ) |>
    gt::tab_spanner(
      label   = gt::md("R^2^"),
      columns = tidyselect::starts_with("r2")
    ) |>
    gt::sub_missing(missing_text = "\u2013") |>
    gt::cols_align(
      align   = "left",
      columns = "model"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"model"
    ) |>
    gt::cols_label(
      model          = "Model",
      n_par          = "Parameters",
      log_lik        = "Log-likelihood",
      lrt            = gt::html("&chi;<sup>2</sup> (df)"),
      p              = "p",
      aic            = "AIC (weight)",
      bic            = "BIC (weight)",
      r2_conditional = "Conditional",
      r2_marginal    = "Marginal"
    ) |>
    gt::tab_footnote(
      footnote = "Weights are normalised against all successfully fitted models.",
      locations = gt::cells_column_spanners("Information criteria")
    ) |>
    gt::tab_style(
      style     = gt::cell_text(indent = gt::px(20)),
      locations = gt::cells_body(columns = "model")
    )
}


#' Table of the stability model estimates
#'
#' One row per set, index and model: the intercept with its Wald 95% CI (on the
#' scale of the model and of the index), the change per test block with its
#' Wald 95% CI, and the SDs of the random effects.
#'
#' @param estimates The `estimates` element of [model_stability()].
#'
#' @returns A `gt` table.
tbl_stability_estimates <- function(estimates) {
  with_ci <- function(estimate, low, high) {
    dplyr::if_else(
      is.na(estimate),
      NA_character_,
      sprintf("%.2f [%.2f, %.2f]", estimate, low, high)
    )
  }
  
  fixed <-
    estimates |>
    dplyr::filter(component == "fixed") |>
    dplyr::mutate(
      term  = dplyr::if_else(term == "(Intercept)", "intercept", "change"),
      value = with_ci(estimate, ci_low, ci_high),
      index_value = with_ci(index_scale, index_low, index_high)
    ) |>
    dplyr::select(
      set,
      index,
      model,
      term,
      value,
      index_value
    ) |>
    tidyr::pivot_wider(
      names_from  = term,
      values_from = c(value, index_value)
    ) |>
    dplyr::select(
      set,
      index,
      model,
      value_intercept,
      index_value_intercept,
      value_change
    )
  
  sds <-
    estimates |>
    dplyr::filter(component == "sd") |>
    dplyr::mutate(
      term = dplyr::case_when(
        term == "Residual" ~ "sd_residual",
        term == "id: (Intercept)" ~ "sd_id",
        term == "id: block0" ~ "sd_id_change",
        term == "pair: (Intercept)" ~ "sd_pair",
        TRUE ~ term
      )
    ) |>
    dplyr::select(
      set,
      index,
      model,
      term,estimate
    ) |>
    tidyr::pivot_wider(
      names_from  = term,
      values_from = estimate
    )
  
  for (col in c("sd_id", "sd_id_change", "sd_pair", "sd_residual")) {
    if (!col %in% names(sds)) sds[[col]] <- NA_real_
  }
  
  fixed |>
    dplyr::left_join(
      sds,
      by = c("set", "index", "model")
    ) |>
    dplyr::mutate(
      group = paste0(
        set, ", ",
        dplyr::if_else(index == "bi_confidence", "BI\u2082", "BI\u2083")
      ),
      model = dplyr::recode(
        model,
        intercept        = "No drift",
        drift            = "Common drift",
        drift_individual = "Individual drift"
      )
    ) |>
    dplyr::select(
      group, model,
      value_intercept, index_value_intercept, value_change,
      sd_id, sd_id_change, sd_pair, sd_residual
    ) |>
    gt::gt(groupname_col = "group") |>
    gt::fmt_number(
      columns  = c("sd_id", "sd_id_change", "sd_pair", "sd_residual"),
      decimals = 2
    ) |>
    gt::sub_missing(missing_text = "\u2013") |>
    gt::tab_style(
      style     = gt::cell_text(whitespace = "nowrap"),
      locations = gt::cells_body(columns = c("value_intercept", "index_value_intercept", "value_change"))
    ) |>
    gt::cols_align(
      align   = "left",
      columns = "model"
    ) |>
    gt::cols_align(
      align   = "center",
      columns = -"model"
    ) |>
    gt::tab_spanner(
      label   = "Intercept",
      columns = c("value_intercept", "index_value_intercept")
    ) |>
    gt::tab_spanner(
      label   = "Participant",
      columns = tidyselect::starts_with("sd_id")
    ) |>
    gt::tab_spanner(
      label   = "Random-effect SD",
      columns = c("sd_id", "sd_id_change", "sd_pair")
    ) |>
    gt::cols_label(
      model                 = "Model",
      value_intercept       = "Model scale",
      index_value_intercept = "Index scale",
      value_change          = "Drift (per block)",
      sd_id                 = "Intercept",
      sd_id_change          = "Drift",
      sd_pair               = "Test pair",
      sd_residual           = gt::html("&sigma;")
    ) |>
    gt::tab_options(
      table.font.size = gt::pct(85)
    ) |>
    gt::tab_footnote(
      footnote  = paste(
        "Residual standard deviation. Applies to Gaussian models only, because",
        "the binomial variance follows from its mean."
      ),
      locations = gt::cells_column_labels("sd_residual")
    ) |>
    gt::tab_style(
      style     = gt::cell_text(indent = gt::px(20)),
      locations = gt::cells_body(columns = "model")
    )
}


#' Figure of the observed and modelled change across test blocks
#'
#' Observed: the mean of the participants' per-block indices with one
#' between-participant standard error each way. Modelled: the fixed-effect
#' prediction of the common-drift model with its Wald 95% band, on the scale of
#' the index (for BI3, the marginal value of [marginal_choice_index()]).
#'
#' @param per_block Per-participant indices per block, from
#'   [catlasr::index_blocking()] with `by_block = TRUE`.
#' @param stability From [model_stability()].
#'
#' @returns A `ggplot`.
fig_stability <- function(per_block, stability) {
  sets <- levels(stability$comparison$set)
  
  observed <-
    purrr::map(sets, function(set) {
      ids <- unique(as.character(stability$fits[[set]]$bi_confidence$intercept@frame$id))
      per_block |>
        dplyr::filter(id %in% ids) |>
        dplyr::select(id, blocks, bi_confidence, bi_choice) |>
        tidyr::pivot_longer(
          cols      = c(bi_confidence, bi_choice),
          names_to  = "index",
          values_to = "value"
        ) |>
        dplyr::filter(!is.na(value)) |>
        dplyr::group_by(index, blocks) |>
        dplyr::summarise(
          mean    = mean(value),
          se      = stats::sd(value) / sqrt(dplyr::n()),
          .groups = "drop"
        ) |>
        dplyr::mutate(set = set, block = as.integer(blocks))
    }) |>
    purrr::list_rbind()
  
  predicted <-
    purrr::map(sets, function(set) {
      purrr::imap(stability$fits[[set]], function(models, index) {
        model <- models$drift
        grid <- data.frame(block0 = 0:5)
        x <- stats::model.matrix(~ block0, grid)
        eta <- as.vector(x %*% lme4::fixef(model))
        se <- sqrt(diag(x %*% as.matrix(stats::vcov(model)) %*% t(x)))
        scale <- if (index == "bi_choice") {
          function(v) marginal_choice_index(model, v)
        } else {
          identity
        }
        tibble::tibble(
          set   = set,
          index = index,
          block = grid$block0 + 1L,
          fit   = scale(eta),
          low   = scale(eta - stats::qnorm(0.975) * se),
          high  = scale(eta + stats::qnorm(0.975) * se)
        )
      }) |>
        purrr::list_rbind()
    }) |>
    purrr::list_rbind()
  
  panel <- function(d) {
    dplyr::mutate(
      d,
      set   = factor(set, levels = sets),
      index = factor(
        index,
        levels = c("bi_confidence", "bi_choice"),
        labels = c(
          bi_confidence = 'BI[2]*":"~"confidence-weighted choice score"',
          bi_choice     = 'BI[3]*":"~"choice-proportion score"'
        )
      )
    )
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = panel(predicted),
      ggplot2::aes(x = block, ymin = low, ymax = high),
      fill  = "grey89",
      alpha = 0.7
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour     = "red3",
      linewidth  = 0.5,
      linetype   = "dotted"
    ) +
    ggplot2::geom_line(
      data = panel(predicted),
      ggplot2::aes(x = block, y = fit),
      linewidth = 0.8,
      colour    = "grey25" 
    ) +
    ggplot2::geom_errorbar(
      data = panel(observed),
      ggplot2::aes(x = block, ymin = mean - se, ymax = mean + se),
      width     = 0.15,
      linewidth = 0.5,
      colour    = "grey35"
    ) +
    ggplot2::geom_point(
      data = panel(observed),
      ggplot2::aes(x = block, y = mean),
      size   = 2,
      colour = "grey20"
    ) +
    ggh4x::facet_grid2(
      rows        = ggplot2::vars(set),
      cols        = ggplot2::vars(index),
      scales      = "free_y",
      independent = "y",
      labeller    = ggplot2::labeller(index = ggplot2::label_parsed)
    ) +
    ggplot2::scale_x_continuous(breaks = 1:6) +
    ggplot2::labs(
      x = "Test block",
      y = "Blocking index"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      strip.background   = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold")
    )
}
