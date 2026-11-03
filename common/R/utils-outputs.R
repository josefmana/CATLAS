#' Folder of the outputs of a manuscript
#'
#' Notebooks save the results that later notebooks need to their manuscript's
#' `outputs/` folder, which is gitignored. The repository root is found from
#' `CATLAS.Rproj`, so the path does not depend on where rendering starts.
#'
#' @param manuscript Character(1). The manuscript folder within `manuscripts/`,
#'   e.g. `"forward-blocking-attention-01"`.
#'
#' @returns Character(1), the path of the folder.
output_dir <- function(manuscript) {
  repo <- rprojroot::find_root(rprojroot::has_file("CATLAS.Rproj"))
  path <- file.path(repo, "manuscripts", manuscript)
  if (!dir.exists(path)) {
    stop(
      sprintf(
        "There is no manuscript '%s' in %s.",
        manuscript,
        file.path(repo, "manuscripts")
      ),
      call. = FALSE
    )
  }
  file.path(path, "outputs")
}


#' Save an output for later notebooks of the same manuscript
#'
#' The folder is not tracked by git, but it is shared with anyone who renders
#' the manuscript, so it holds aggregates only, never participant-level data.
#'
#' @param object The R object to save.
#' @param file Character(1). File name, starting with the number of the
#'   notebook that writes it, e.g. `"02-feasibility-estimates.rds"`.
#' @inheritParams output_dir
#'
#' @returns The path of the saved file, invisibly.
write_output <- function(object, file, manuscript) {
  dir <- output_dir(manuscript)
  dir.create(dir, showWarnings = FALSE)
  path <- file.path(dir, file)
  saveRDS(object, path)
  invisible(path)
}


#' Read an output saved by another notebook of the same manuscript
#'
#' @inheritParams write_output
#'
#' @returns The object stored in `file`.
read_output <- function(file, manuscript) {
  dir <- output_dir(manuscript)
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
