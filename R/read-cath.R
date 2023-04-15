cols_cath_resolve_hits_summarised <- readr::cols(
  query.id     = readr::col_character(),
  target.id    = readr::col_character(),
  score        = readr::col_double(),
  boundaries   = readr::col_character(),
  resolved     = readr::col_character(),
  query.length = readr::col_double()
)

# TODO: Write documentation.
#' @title Read a summarised table from `cath-resolve-hits`
#' @description Read a summarised table from `cath-resolve-hits`.
#' @param path Path to file.
#' @return A [tibble::tibble()]
#' @export
read_cath_resolve_hits_summarised <- function(path) {
  cols <- cols_cath_resolve_hits_summarised
  out <- readr::read_delim(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE)
  out
}

# TODO: Write documentation.
#' @title Process a summarised `cath-resolve-hits` table
#' @description Process a summarised `cath-resolve-hits` table.
#' @param cath_resolve_hits_summarised As read-in by [read_cath_resolve_hits_summarised()].
#' @return A [tibble::tibble()]
#' @export
cath_resolve_hits_process <- function(cath_resolve_hits_summarised) {
  separated_boundaries <- tidyr::separate_wider_delim(
    cath_resolve_hits_summarised,
    cols = boundaries,
    delim = "-",
    names = c("query.start", "query.end")
  )
  separated_resolved <- tidyr::separate_wider_delim(
    separated_boundaries,
    cols = resolved,
    delim = "-",
    names = c("resolved.start", "resolved.end")
  )
  out <- dplyr::mutate(
    separated_resolved,
    dplyr::across(
      c("query.start", "query.end", "resolved.start", "resolved.end"),
      as.double
    )
  )
  out
}
