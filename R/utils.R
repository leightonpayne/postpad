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

#' Read multiple files
#' @param path A character vector of full path names; the default corresponds
#' to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#' performed. Missing values will be ignored. Elements with a marked encoding
#' will be converted to the native encoding (and if that fails, considered
#' non-existent).
#' @param func A function to use for reading.
#' @param pattern An optional regular expression. Only file names which match
#' the regular expression will be returned.
#' @param name The name of the progress bar to display.
#' @return A [list()], where each element is a [tibble::tibble()] holding
#' the contents of the file.
#' @export
multi_read <- function(path = ".", func, pattern = NULL, name) {
  func <- substitute(func)
  file_paths <- list.files(path, full.names = TRUE, pattern = pattern)
  # file_content <- purrr::map(.x = file_paths, .f = function(x) eval(func)(x))
  file_content <- purrr::map(.x = cli::cli_progress_along(file_paths, name = name), .f = function(i) eval(func)(file_paths[i]))
  file_names <- basename(file_paths)
  names(file_content) <- file_names
  file_content
}
