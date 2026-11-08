#' Text of a pre-registration item
#'
#' Interpolates the R expressions between `{{` and `}}`, then joins the lines
#' of each paragraph, so that the item reads as continuous prose wherever its
#' text is pasted (the rendered form, the registry, the JSON export). Blank
#' lines separate paragraphs, and lines starting with `- ` or `1. ` read as
#' list items.
#'
#' @param ... Character. The text, passed to [glue::glue()].
#' @param .envir Environment in which the expressions are evaluated.
#'
#' @returns Character(1).
prereg_text <- function(..., .envir = parent.frame()) {
  x <- glue::glue(..., .envir = .envir, .open = "{{", .close = "}}")
  x <- gsub("(?m)^[ \\t]+|[ \\t]+$", "", x, perl = TRUE)
  x <- gsub("(?<=\\S)\\n(?!\\n|- |[0-9]+\\. )", " ", x, perl = TRUE)
  trimws(as.character(x))
}


#' Exclusion rules of the registered study
#'
#' Lists the exclusion rules in the order they are applied, with their wording
#' taken verbatim from a provenance record, except for the rules whose wording
#' the registered study revises.
#'
#' @param exclusions The `exclusions` element of a `{catlasr}` provenance
#'   record, i.e., a list with one element per rule, each with `rule` and
#'   `description`.
#' @param rule_labels Named character. The label of every rule, named by rule
#'   and in the order of `exclusions`.
#' @param revised Named character. The new wording of the revised rules, named
#'   by rule.
#'
#' @returns Character, one Markdown list item per rule.
write_prereg_rules <- function(
    exclusions,
    rule_labels,
    revised = character()
) {
  rules <- purrr::map_chr(exclusions, "rule")
  stopifnot(
    identical(unname(rules), names(rule_labels)),
    all(names(revised) %in% rules)
  )
  
  descriptions <- rlang::set_names(purrr::map_chr(exclusions, "description"), rules)
  descriptions[names(revised)] <- revised
  
  sprintf(
    "%d. **%s.** %s",
    seq_along(rules),
    rule_labels[rules],
    descriptions
  )
}


#' Decision tree of the registered study as a list
#'
#' @param decisions A tibble with one row per look and combination of test
#'   results: `look`, `bi2` and `bi3` (`"pass"` or `"fail"`), `replication`,
#'   `next_step` and `note`, as exported by the power analysis notebook.
#' @param thresholds Character(2). The one-sided significance thresholds of
#'   looks 1 and 2 as printed, e.g. `c("< .045", "< .007")`.
#'
#' @returns Character, one Markdown list item per row of `decisions`.
write_prereg_decisions <- function(decisions, thresholds) {
  outcome <- function(x) dplyr::if_else(x == "pass", "passes", "fails")
  
  study_names <- function(x) {
    x |>
      stringr::str_replace_all("(?<![\\w-])-03\\b", "`fba-allergy-online-03`") |>
      stringr::str_replace_all("BI([23])", "BI~\\1~")
  }
  
  note <- dplyr::if_else(
    is.na(decisions$note),
    "",
    paste0(" (", study_names(decisions$note), ")")
  )
  
  sprintf(
    "- **Look %d** (one-sided *p* %s): BI~2~ %s, BI~3~ %s. Replication: **%s**. Next step: %s%s.",
    decisions$look,
    thresholds[decisions$look],
    outcome(decisions$bi2),
    outcome(decisions$bi3),
    decisions$replication,
    study_names(decisions$next_step),
    note
  )
}


#' Contributors of a pre-registration as text
#'
#' @param contributors A tibble with one row per contributor and the columns
#'   `name`, `affiliation`, `department`, `orcid` and `role` (one of
#'   `"Author(s) / Creator(s)"`, `"Advisor(s)"` or
#'   `"Other kind(s) of contributor"`).
#'
#' @returns Character(1), one Markdown list item per contributor.
write_prereg_contributors <- function(contributors) {
  paste(
    sprintf(
      "- **%s** (ORCID %s), %s, %s. Contributor role: %s.",
      contributors$name,
      contributors$orcid,
      contributors$department,
      contributors$affiliation,
      contributors$role
    ),
    collapse = "\n"
  )
}


#' Items of a pre-registration with their answers
#'
#' @param x A `{preregr}` (pre)registration object, as returned by
#'   [preregr::prereg_initialize()] and filled by [preregr::prereg_specify()].
#'
#' @returns A tibble with one row per item: the item columns of the form
#'   (without the `item_` prefix), `text` (`NA` when unspecified), and `shown`,
#'   whether the web form shows the item given the answers.
prereg_items <- function(x) {
  items <- tibble::as_tibble(x$form$items)
  names(items) <- sub("^item_", "", names(items))

  text <- purrr::map_chr(items$id, \(id) x$specs[[id]]$text)
  text[text == x$config$initialText] <- NA_character_
  items$text <- text

  if (!"shown_if_id" %in% names(items)) {
    items$shown <- TRUE
    return(items)
  }

  # An item is shown if it has no condition, or if the item it depends on is
  # shown and holds (or, with several options, includes) the value
  holds <- function(id, value) {
    answer <- items$text[match(id, items$id)]
    if (is.na(answer)) return(FALSE)
    value %in% trimws(strsplit(answer, ";", fixed = TRUE)[[1L]])
  }
  shown <- is.na(items$shown_if_id)
  repeat {
    previous <- shown
    shown <- is.na(items$shown_if_id) | purrr::map2_lgl(
      items$shown_if_id, items$shown_if_value,
      \(id, value) !is.na(id) && shown[match(id, items$id)] && holds(id, value)
    )
    if (identical(shown, previous)) break
  }
  items$shown <- shown

  items
}


#' Completion of the pre-registration form
#'
#' @param x A `{preregr}` (pre)registration object.
#' @param flag Character. Markers of content still to be decided or added.
#'
#' @returns A tibble with one row per item that needs attention: `item`,
#'   `label` and `status`, which is one of "Required, unspecified" (a required
#'   item that the web form shows), "Optional, unspecified", "Hidden but
#'   specified" (an item that the web form does not show given the answers, so
#'   its text would be lost), or "To decide" (text holding one of `flag`).
check_prereg <- function(x, flag = c("[To decide", "[To add")) {
  items <- prereg_items(x)
  required <- if ("required" %in% names(items)) items$required else TRUE
  flagged <- purrr::map_lgl(
    items$text,
    \(t) !is.na(t) && any(vapply(flag, \(f) grepl(f, t, fixed = TRUE), logical(1)))
  )

  tibble::tibble(
    item   = items$id,
    label  = items$label,
    status = dplyr::case_when(
      !items$shown & !is.na(items$text) ~ "Hidden but specified",
      !items$shown ~ NA_character_,
      is.na(items$text) & required ~ "Required, unspecified",
      is.na(items$text) ~ "Optional, unspecified",
      flagged ~ "To decide",
      TRUE ~ NA_character_
    )
  ) |>
    dplyr::filter(!is.na(status))
}


#' Path to the Quarto command-line tool
#'
#' Quarto bundles the pandoc and typst binaries that the conversions and the
#' PDF export use.
#'
#' @returns Character(1), the path to the `quarto` executable.
find_quarto <- function() {
  candidates <- c(
    Sys.getenv("QUARTO_PATH"),
    if (requireNamespace("quarto", quietly = TRUE)) {
      tryCatch(quarto::quarto_path(), error = function(e) "")
    },
    Sys.which("quarto"),
    "/Applications/quarto/bin/quarto",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/usr/local/bin/quarto",
    "/opt/quarto/bin/quarto"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    stop(
      "Quarto was not found. Set the QUARTO_PATH environment variable to the ",
      "path of the quarto executable."
    )
  }
  unname(candidates[[1L]])
}


#' Run a Quarto subcommand
#'
#' @param args Character. The arguments, e.g. `c("pandoc", "in.md", ...)`.
#'
#' @returns Invisibly, the standard output. Stops with the error output if the
#'   command fails.
run_quarto <- function(args) {
  err <- tempfile(fileext = ".txt")
  out <- suppressWarnings(system2(
    find_quarto(),
    args = shQuote(args),
    stdout = TRUE,
    stderr = err
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "Quarto failed (quarto ", paste(args, collapse = " "), "):\n",
      paste(readLines(err, warn = FALSE), collapse = "\n")
    )
  }
  invisible(out)
}


#' Citation keys of a pre-registration
#'
#' @param x A `{preregr}` (pre)registration object.
#' @param bibliography Character(1). Path to the BibTeX file.
#'
#' @returns Character, the keys of the bibliography that the items cite (as
#'   `@key`), in order of first citation.
prereg_cited_keys <- function(x, bibliography) {
  items <- prereg_items(x)
  text <- paste(stats::na.omit(items$text[items$shown]), collapse = "\n")
  cited <- regmatches(
    text,
    gregexpr("(?<![\\w.@])@([A-Za-z0-9_][A-Za-z0-9_:.#$%&+?<>~/-]*[A-Za-z0-9_])", text, perl = TRUE)
  )[[1L]]
  cited <- unique(sub("^@", "", cited))

  bib <- readLines(bibliography, warn = FALSE, encoding = "UTF-8")
  keys <- sub("^\\s*@\\w+\\s*\\{\\s*([^,]+),.*$", "\\1", grep("^\\s*@\\w+\\s*\\{", bib, value = TRUE))
  missing <- setdiff(cited, keys)
  if (length(missing) > 0L) {
    warning(
      "Cited but not in the bibliography: ",
      paste0("@", missing, collapse = ", ")
    )
  }
  intersect(cited, keys)
}


#' Convert Markdown texts with pandoc, resolving citations
#'
#' @param texts Named character. The Markdown texts, e.g. item answers.
#' @param bibliography Character(1). Path to the BibTeX file.
#' @param csl Character(1). Path to the citation style.
#' @param to Character(1). The pandoc output format, e.g. `"plain"` or
#'   `"markdown"`.
#'
#' @returns A list: `texts` (named character, the converted texts) and
#'   `references` (character(1), the list of the cited references).
prereg_convert <- function(texts, bibliography, csl, to = "plain") {
  marker <- "PREREGITEMMARKER"
  body <- paste0(
    marker, " ", names(texts), "\n\n", texts,
    collapse = "\n\n"
  )
  # A subscript z has no Unicode character; plain text writes d_z instead of
  # pandoc's d_(z)
  if (to == "plain") {
    body <- gsub("~z~", "_z", body, fixed = TRUE)
  }
  md <- paste0(
    body, "\n\n", marker, " REFLIST\n\n::: {#refs}\n:::\n"
  )

  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".txt")
  writeLines(md, input, useBytes = TRUE)
  run_quarto(c(
    "pandoc", input,
    "--from", "markdown",
    "--to", to,
    "--citeproc",
    "--bibliography", normalizePath(bibliography),
    "--csl", normalizePath(csl),
    "--wrap", "none",
    "--output", output
  ))

  lines <- readLines(output, warn = FALSE, encoding = "UTF-8")
  starts <- grep(paste0("^\\\\?", marker, " "), lines)
  ids <- sub(paste0("^\\\\?", marker, " "), "", lines[starts])
  ids <- gsub("\\\\", "", ids)
  ends <- c(starts[-1L] - 1L, length(lines))
  chunks <- purrr::map2_chr(starts, ends, \(s, e) {
    if (e <= s) return("")
    out <- paste(lines[(s + 1L):e], collapse = "\n")
    # Pandoc's Markdown writer wraps the reference list in fenced divs
    out <- gsub("(?m)^:{3,}.*$", "", out, perl = TRUE)
    trimws(gsub("\n{3,}", "\n\n", out))
  })
  names(chunks) <- ids

  list(
    texts = chunks[setdiff(ids, "REFLIST")],
    references = chunks[["REFLIST"]]
  )
}


#' Reference list of a pre-registration
#'
#' @inheritParams prereg_cited_keys
#' @param csl Character(1). Path to the citation style, e.g. APA.
#'
#' @returns Character(1), the formatted references in Markdown, one paragraph
#'   per reference.
prereg_references <- function(x, bibliography, csl) {
  keys <- prereg_cited_keys(x, bibliography)
  if (length(keys) == 0L) {
    return("")
  }
  cites <- purrr::set_names(paste0("[@", keys, "]"), keys)
  prereg_convert(cites, bibliography, csl, to = "markdown")$references
}


#' A pre-registration as a Markdown document
#'
#' Lists the items the web form shows, by section, with their template number,
#' label and answer. Unspecified items are marked as such; the citations stay
#' as `@key`, to be resolved by whatever renders the document.
#'
#' @param x A `{preregr}` (pre)registration object.
#' @param exclude Character. Items to leave out, e.g. `"R1"` when the renderer
#'   builds the reference list itself.
#' @param heading_level Integer(1). The level of the section headings.
#' @param item_style Character(1). `"heading"` gives every item a heading one
#'   level below its section; `"bold"` a bold label, which keeps the items out
#'   of a table of contents.
#'
#' @returns Character(1).
prereg_to_markdown <- function(
    x,
    exclude = character(),
    heading_level = 1L,
    item_style = c("heading", "bold")
) {
  item_style <- match.arg(item_style)
  items <- prereg_items(x)
  items <- items[items$shown & !items$id %in% exclude, ]
  sections <- x$form$sections
  title <- if ("title" %in% names(items)) items$title else items$label
  number <- if ("number" %in% names(items)) items$number else items$id
  required <- if ("required" %in% names(items)) items$required else FALSE
  h1 <- strrep("#", heading_level)
  h2 <- strrep("#", heading_level + 1L)

  blocks <- purrr::map_chr(seq_len(nrow(items)), function(i) {
    answer <- if (is.na(items$text[i])) "*Unspecified.*" else items$text[i]
    if (identical(items$type[i], "checkboxgroup") && !is.na(items$text[i])) {
      answer <- paste0("- ", trimws(strsplit(answer, ";", fixed = TRUE)[[1L]]), collapse = "\n")
    }
    label <- paste0(
      if (nzchar(number[i])) paste0(number[i], " ") else "",
      title[i],
      if (isTRUE(required[i])) " \u2605" else ""
    )
    if (item_style == "heading") {
      sprintf("%s %s {.unnumbered}\n\n%s", h2, label, answer)
    } else {
      sprintf("**%s**\n\n%s", label, answer)
    }
  })

  section_of <- items$section_id
  out <- character()
  for (s in unique(section_of)) {
    label <- sections$section_label[match(s, sections$section_id)]
    out <- c(out, sprintf("%s %s {.unnumbered}", h1, label), blocks[section_of == s])
  }
  paste(out, collapse = "\n\n")
}


#' Export a pre-registration to PDF
#'
#' Converts the Markdown of [prereg_to_markdown()] with pandoc to typst,
#' resolving the citations and appending the reference list, and compiles it
#' with typst, both as bundled with Quarto.
#'
#' @param x A `{preregr}` (pre)registration object.
#' @param file Character(1). The PDF file to write.
#' @param title,subtitle Character(1). The title and subtitle of the document.
#' @param bibliography Character(1). Path to the BibTeX file.
#' @param csl Character(1). Path to the citation style.
#' @param contributors Optional tibble of contributors, as for
#'   [write_prereg_contributors()], named as authors.
#'
#' @returns Invisibly, `file`.
prereg_to_pdf <- function(
    x,
    file,
    title,
    subtitle = NULL,
    bibliography,
    csl,
    contributors = NULL
) {
  stopifnot(dir.exists(dirname(file)))

  yaml_string <- function(s) paste0("\"", gsub("\"", "\\\\\"", s), "\"")
  header <- c(
    "---",
    paste0("title: ", yaml_string(title)),
    if (!is.null(subtitle)) {
      paste0("subtitle: ", yaml_string(subtitle))
    },
    if (!is.null(contributors)) {
      c("author:", paste0("  - ", yaml_string(contributors$name)))
    },
    paste0(
      "date: ", yaml_string(format(Sys.Date(), "%d %B %Y"))
    ),
    "papersize: a4",
    "---"
    )
  body <- prereg_to_markdown(x, exclude = "R1")
  md <- paste(
    c(header, "", body, "", "# References {.unnumbered}", "", "::: {#refs}", ":::"),
    collapse = "\n"
  )

  dir <- tempfile("prereg-pdf-")
  dir.create(dir)
  input <- file.path(dir, "prereg.md")
  typ <- file.path(dir, "prereg.typ")
  pdf <- file.path(dir, "prereg.pdf")
  writeLines(md, input, useBytes = TRUE)

  run_quarto(c(
    "pandoc", input,
    "--from", "markdown",
    "--to", "typst",
    "--standalone",
    "--citeproc",
    "--bibliography", normalizePath(bibliography),
    "--csl", normalizePath(csl),
    "--output", typ
  ))
  run_quarto(c("typst", "compile", typ, pdf))

  file.copy(pdf, file, overwrite = TRUE)
  invisible(file)
}


#' Export a pre-registration as a state file of the PreReg web form
#'
#' Writes the `.rds` file that the PreReg web form of the PRP-QUANT Template V4
#' saves with "Save your current state" and reads with "Import previous state":
#' a list of `app`, `template_version`, `contributors` and `answers`. The
#' answers are the items' texts converted to plain text, with the citations
#' resolved; R1 holds the reference list. Items marked with the form's
#' not-applicable text are exported empty, with their "NA" checkbox ticked.
#' Items the web form would not show given the answers are exported empty.
#'
#' @param x A `{preregr}` (pre)registration object filled in the PRP-QUANT V4
#'   form of [form_prpquant_v4()].
#' @param file Character(1). The `.rds` file to write.
#' @param contributors A tibble of contributors, as for
#'   [write_prereg_contributors()].
#' @param bibliography Character(1). Path to the BibTeX file.
#' @param csl Character(1). Path to the citation style.
#'
#' @returns Invisibly, the state list.
prereg_to_zpid <- function(x, file, contributors, bibliography, csl) {
  items <- prereg_items(x)
  stopifnot(
    "type" %in% names(items),
    all(c("name", "affiliation", "department", "orcid", "role") %in% names(contributors))
  )
  meta <- x$form$metadata
  na_text <- meta$content[meta$field == "na_text"]
  if (length(na_text) == 0L) na_text <- "Not applicable."

  items <- items[items$id != "contributors", ]
  is_na <- !is.na(items$text) & items$text == na_text
  is_text <- items$type %in% c("text", "textarea")

  # Free text in plain form, with citations resolved; R1 gets the references
  convert <- items$shown & is_text & !is.na(items$text) & !is_na & items$id != "R1"
  converted <- prereg_convert(
    purrr::set_names(items$text[convert], items$id[convert]),
    bibliography, csl, to = "plain"
  )
  references <- prereg_convert(
    purrr::set_names(
      paste0("[@", prereg_cited_keys(x, bibliography), "]"),
      prereg_cited_keys(x, bibliography)
    ),
    bibliography, csl, to = "plain"
  )$references

  answer <- function(i) {
    id <- items$id[i]
    text <- items$text[i]
    type <- items$type[i]
    if (!items$shown[i] || is.na(text) || is_na[i]) {
      return(switch(type, radio = NULL, checkboxgroup = NULL, checkbox = FALSE, ""))
    }
    switch(
      type,
      radio = text,
      checkboxgroup = trimws(strsplit(text, ";", fixed = TRUE)[[1L]]),
      checkbox = identical(text, "Yes"),
      if (id == "R1") references else unname(converted$texts[[id]])
    )
  }

  headers <- v4_header_keys()
  answers <- list()
  for (i in seq_len(nrow(items))) {
    id <- items$id[i]
    if (id %in% names(headers)) {
      answers[headers[[id]]] <- list(NULL)
    }
    answers[id] <- list(answer(i))
  }
  answers["R2"] <- list(NULL)
  answers["R3"] <- list(NULL)
  for (i in which(items$na)) {
    answers[[paste0(items$id[i], "__na")]] <- is_na[i]
  }

  state <- list(
    app = "PRP-QUANT",
    template_version = "4",
    contributors = purrr::pmap(
      contributors[c("name", "affiliation", "department", "orcid", "role")],
      \(name, affiliation, department, orcid, role) list(
        `Full name`        = name,
        Affiliation        = affiliation,
        Department         = department,
        ORCID              = orcid,
        `Contributor role` = role
      )
    ),
    answers = answers
  )

  saveRDS(state, file)
  invisible(state)
}
