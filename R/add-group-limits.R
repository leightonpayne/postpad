#' Add minimum and maximum values for columns by groups
#'
#' This function adds two new columns to a data frame. The first column contains
#' the minimum value of a specified column for each group, and the second column
#' contains the maximum value of another specified column for each group.
#'
#' @param df A data frame.
#' @param col1 A column name for which to calculate the minimum value.
#' @param col2 A column name for which to calculate the maximum value.
#' @param .by A single grouping variable, or a list of grouping variables.
#'
#' @return A data frame with two new columns: one containing the minimum value
#' of col1 for each group, and the other containing the maximum value of col2
#' for each group
#'
#' @export
add_group_limits <- function(df, col1, col2, .by) {
  # Use the last grouping variable as a prefix for the new column names.
  prefix <- tail(.by, 1)
  # substitute() is used to capture the names of col1 and col2 as expressions
  # rlang::as_string() is used to convert those expressions to strings
  col1_str <- rlang::as_string(substitute(col1))
  col2_str <- rlang::as_string(substitute(col2))
  min_col <- glue::glue("min_{prefix}_{col1_str}")
  max_col <- glue::glue("max_{prefix}_{col2_str}")
  col_limits <- dplyr::mutate(
    df,
    # !! and := are used to inject the values of min_col and max_col as column
    # names {{}} is used to forward the unquoted col1 and col2 arguments to
    # mutate()
    !!min_col := min({{ col1 }}),
    !!max_col := max({{ col2 }}),
    .by = .by
  )
  col_limits
}

# CAN'T MAKE THIS WORK FOR SOME REASON
# #' Add minimum and maximum limits for columns based on groups
# #'
# #' This function adds two new columns to a data frame. The first column contains
# #' the minimum value of a specified column for each group, and the second column
# #' contains the maximum value of another specified column for each group.
# #'
# #' @param df A data frame
# #' @param col1 A column name for which to calculate the minimum value
# #' @param col2 Optionally, a column name for which to calculate the maximum value
# #' @param .by A single grouping variable, or a list of grouping variables
# #'
# #' @return A data frame with two new columns: one containing the minimum value of
# #' col1 for each group, and the other containing the maximum value of col2 for each group
# #'
# #' @export
# add_group_limits_1 <- function(df, col1, col2 = NULL, .by) {
#   # Use the last grouping variable as a prefix for the new column names.
#   prefix <- tail(.by, 1)
#   #
#   col1_str <- rlang::as_string(substitute(col1))
#   col2_str <- if (is.null(col2)) col1_str else rlang::as_string(substitute(col2))
#   # substitute() is used to get the unevaluated expressions for col1 and col2.
#   # This allows us to programmatically generate new column names based on the
#   # grouping variable.
#   min_col <- glue::glue("min_{prefix}_{col1_str}")
#   max_col <- glue::glue("max_{prefix}_{col2_str}")
#   # # !! and := is used to inject the values of col1_min and col2_max.
#   # # {{ is used to refer to the column names as unquoted expressions.
#   if (is.null(col2)) {
#     col_limits <- dplyr::mutate(
#       df,
#       !!min_col := min({{col1}}),
#       !!max_col := max({{col1}}),
#       .by = .by
#     )
#   } else {
#     col_limits <- dplyr::mutate(
#       df,
#       !!min_col := min({{col1}}),
#       !!max_col := max({{col2}}),
#       .by = .by
#     )
#   }
#
#   col_limits
# }
