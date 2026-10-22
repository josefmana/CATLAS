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
    characteristic = c(
      "N",
      "Age, mean (SD)",
      "Age, range",
      paste0("Sex: ", names(sex)),
      "Prior knowledge reported"
    ),
    value = c(
      as.character(nrow(m)),
      sprintf("%.1f (%.1f)", mean(m$age, na.rm = TRUE), stats::sd(m$age, na.rm = TRUE)),
      sprintf("%.0f\u2013%.0f", min(m$age, na.rm = TRUE), max(m$age, na.rm = TRUE)),
      as.character(as.integer(sex)),
      as.character(sum(m$prior_knowledge %in% TRUE))
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
    by = "characteristic"
  ) |>
    dplyr::mutate(dplyr::across(
      .cols = c(full, primary),
      .fns  = \(v) tidyr::replace_na(v, "0")
    )) |>
    gt::gt() |>
    gt::cols_align(
      align   = "center",
      columns = -"characteristic"
    ) |>
    gt::cols_label(
      characteristic = "",
      full           = "Full sample",
      primary        = "Primary set"
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

  scores |>
    dplyr::mutate(type = dplyr::if_else(id %in% ids_failed, paste0(type, "_failed"), type)) |>
    catlasr::plot_learning(level = "participant") +
    ggplot2::scale_colour_manual(
      values = learning_colours,
      breaks = c("blocking", "control", "legit", "filler"),
      name   = NULL
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
    gt::gt() |>
    gt::fmt_number(
      columns  = "alpha",
      decimals = 3
    ) |>
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
      label   = "Kept",
      columns = tidyselect::starts_with("kept")
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = alpha == applied)
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


#' Table of the analysis sets
#'
#' @param meta_screened Metadata of the primary set.
#' @param ids_structural Character. Identifiers of the full sample.
#' @param ids_labels Character. Identifiers recruited through LABELS.
#'
#' @returns A `gt` table.
tbl_analysis_sets <- function(meta_screened, ids_structural, ids_labels) {
  tibble::tibble(
    set    = c("1 \u00b7 Primary", "2 \u00b7 Full sample", "3 \u00b7 Primary, no prior knowledge", "4 \u00b7 Primary, LABELS only"),
    data   = c("After all exclusions", "Before the learning-based exclusion", "After all exclusions", "After all exclusions"),
    filter = c("None", "None", "Prior knowledge not reported", "LABELS batch"),
    n      = c(
      nrow(meta_screened),
      length(ids_structural),
      sum(!(meta_screened$prior_knowledge %in% TRUE)),
      sum(as.character(meta_screened$id) %in% ids_labels)
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
