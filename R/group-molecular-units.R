#' Assign rows to groups based on a single variable describing position
#'
#' For example, grouping genetic units such as genes or systems based on their
#' relative position in the genome.
#'
#' @param df A data frame.
#' @param col The column containing position information.
#' @param n The maximum difference between positions for grouping.
#' @param name The name of the new grouping column.
#' @param by Columns to group by.
#' @export
group_by_position <- function(df, col, n, name, .by) {
  out <- dplyr::mutate(
    df,
    !!name := cumsum(c(1, abs(diff({{col}})) > n)),
    .by = .by
  )
  out
}

#' Assign rows to groups based on two variables describing position
#' (e.g. start and end).
#'
#' For example, grouping genetic units such as genes or systems based on the
#' number of nucleotides between where one unit ends and the next begins.
#'
#' @param df A data frame.
#' @param col1 The column containing start position information
#' @param col2 The column containing end position information
#' @param n The maximum difference between positions for grouping.
#' @param name The name of the new grouping column.
#' @param by Columns to group by.
#' @export
group_by_distance <- function(df, col1, col2, n, name, .by) {
  df <- dplyr::arrange(df, {{col1}})
  out <- dplyr::mutate(
    df,
    !!name := cumsum({{col1}} - dplyr::lag({{col2}}, default = -Inf) > n),
    .by = .by
  )
  out
}

# THIS DOESN'T WORK

# #' Assign rows to groups based on overlapping values in a variable describing
# #' position.
# #'
# #' For example, identifying systrms that have any overlapping components.
# #'
# #' @param df A data frame.
# #' @param col The column containing position information.
# #' @param n The maximum difference between positions for grouping.
# #' @param name The name of the new grouping column.
# #' @param prefix A character vector to prefix the id values in the new grouping column.
# #' @param by Columns to group by.
# #' @export
# group_by_overlap <- function(df, col, name, prefix = "", .by) {
#   out <- dplyr::mutate(
#     df,
#     !!name := paste0(prefix, dplyr::dense_rank(cut({{col}}, dplyr::n_distinct({{col}}), labels = FALSE))),
#     .by = .by
#   )
#   out
# }
