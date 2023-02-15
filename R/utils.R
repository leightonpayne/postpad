#' Combine two [readr::cols()] specifications
#' @param cols1 A [readr::cols()] object.
#' @param cols2 A [readr::cols()] object.
#' @return A [readr::cols()] object of cols1 and cols2 combined.
cols_2c <- function(cols1, cols2) {
  cols_out <- readr::cols()
  cols_out$cols <- c(cols1$cols, cols2$cols)
  cols_out
}

#' Combine two or more [readr::cols()] specifications
#' @param cols A [list()] of [readr::cols()] objects.
#' @return A [readr::cols()] object of all cols combined.
cols_c <- function(cols) {
  cols_out <- purrr::reduce(.x = cols, .f = cols_2c)
  cols_out
}
