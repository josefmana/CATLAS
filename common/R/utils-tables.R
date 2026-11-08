#' Format a gt table in APA 7 style
#'
#' Three-line borders: above and below the column labels and below the body;
#' no internal or outer borders. The title is rendered in italics, flush left.
#' Adapted from
#' <https://www.anthonyschmidt.co/post/2020-06-03-making-apa-tables-with-gt/>.
#'
#' @param x A `gt_tbl` from `gt::gt()`.
#' @param title Character(1). Table title; a single space shows none.
#'
#' @returns `x` with APA 7 formatting, still pipeable.
apa_style <- function(x, title = " ") {
  x |>
    gt::tab_options(
      table.border.top.color = "white",
      heading.title.font.size = gt::px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "white"
    ) |>
    gt::cols_align(align = "center") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = gt::px(1)
        ),
        gt::cell_text(align = "center"),
        gt::cell_fill(color = "white")
      ),
      locations = gt::cells_body()
    ) |>
    gt::tab_header(title = gt::html("<i>", title, "</i>")) |>
    gt::opt_align_table_header(align = "left")
}
