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
#' @param namefix An optional regular expression. The name of each element
#' in the resulting list is derived from the file names. This regex is used to
#' remove a string from each file name before naming the elements.
#' @param barname The name of the progress bar to display.
#' @return A [list()], where each element is a [tibble::tibble()] holding
#' the contents of the file.
#' @export
multi_read <- function(path = ".", func, pattern = NULL, namefix = NULL, barname) {
  func <- substitute(func)
  file_paths <- list.files(path, full.names = TRUE, pattern = pattern)
  file_content <- purrr::map(
    .x = cli::cli_progress_along(file_paths, name = barname),
    .f = function(i) eval(func)(file_paths[i])
  )
  file_names <- basename(file_paths)
  if (!is.null(namefix)) {
    file_names <- stringr::str_remove(file_names, namefix)
  }
  names(file_content) <- file_names
  file_content
}

#' @export
mutate_proportion <- function(.data,
                              col,
                              name = "proportion",
                              .by = NULL,
                              .keep = c("all", "used", "unused", "none")) {
  out <- dplyr::mutate(
    .data = .data,
    {{name}} := {{col}} / sum({{col}}),
    .by = .by, .keep = .keep
    )
  out
}

#' @export
mutate_cur_group_id <- function(.data,
                                name = "cur_group_id",
                                .by = NULL,
                                .keep = c("all", "used", "unused", "none")) {
  out <- dplyr::mutate(
    .data = .data,
    {{name}} := dplyr::cur_group_id(),
    .by = .by,
    .keep = .keep
  )
  out
}

#' @export
mutate_system_id <- function(padlocout, name = "system_id") {
  system_id_group <- c("genome.accession", "seqid", "system", "system.number")
  padlocout_ids <- mutate_cur_group_id(
    padlocout, name = {{ name }}, .by = system_id_group
  )
  padlocout_ids
}

#' @export
summarise_n <- function(.data, name = "n", .by = NULL, .groups = NULL) {
  out <- dplyr::summarise(.data = .data, {{name}} := dplyr::n(), .by = .by, .groups = .groups)
  out
}

#' @export
summarise_proportion <- function(.data, name = "proportion", .by = NULL, .groups = NULL, .keep = c("all", "used", "unused", "none")) {
  n <- summarise_n(.data = .data, name = "n", .by = .by, .groups = .groups)
  out <- mutate_proportion(.data = n, col = n, name = {{ name }}, .by = NULL, .keep = .keep)
  out
}

#' @export
a4dim <- function(n = 1) {
  l <- 246.2
  w <- 159.2
  d <- glue::glue('{w * n} mm x {l * n} mm')
  d
}
