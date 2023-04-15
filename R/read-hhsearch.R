cols_hhsearch_hhprob <- readr::cols(
  query.id   = readr::col_character(),
  target.id  = readr::col_character(),
  prob       = readr::col_double(),
  boundaries = readr::col_character()
)

# TODO: Write documentation.
#' @title Read a HHsearch hhprob table
#' @description Read a HHsearch hhprob table.
#' @param path Path to file.
#' @return A [tibble::tibble()].
#' @export
read_hhsearch_hhprob <- function(path) {
  cols <- cols_hhsearch_hhprob
  out <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE)
}

#' @title Process a HHsearch hhprob table
#' @description Process a HHsearch hhprob table.
#' @param hhsearch_hhprob As read-in by [read_hhsearch_hhprob()].
#' @return A [tibble::tibble()].
#' @export
hhsearch_hhprob_process <- function(hhsearch_hhprob) {
  boundaries_separated <- tidyr::separate_wider_delim(
    hhsearch_hhprob,
    delim = "-",
    cols = boundaries,
    names = c("query.start", "query.end")
  )
  out <- dplyr::mutate(
    boundaries_separated,
    dplyr::across(c("query.start", "query.end"), as.double)
  )
  out
}

cols_hhsearch_hhtbl <- readr::cols(
  query = readr::col_character(),
  target = readr::col_character(),
  target_coverage = readr::col_character(),
  ali_len = readr::col_character(),
  no_mismatch = readr::col_character(),
  no_gap_open = readr::col_character(),
  query_start = readr::col_character(),
  query_end = readr::col_character(),
  target_start = readr::col_character(),
  target_end = readr::col_character(),
  eval = readr::col_character(),
  score = readr::col_character()
)

# TODO: Write documentation.
#' @title Read a HHsearch BLAST-formatted output table
#' @description Read a HHsearch BLAST-formatted output table.
#' @param path Path to file.
#' @return A [tibble::tibble()].
#' @export
read_hhsearch_hhtbl <- function(path) {
  cols <- cols_hhsearch_hhtbl
  out <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE)
}

