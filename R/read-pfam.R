cols_pfam_clans <- readr::cols(
  family.id   = readr::col_character(),
  clan.id     = readr::col_character(),
  clan.name   = readr::col_character(),
  family.name = readr::col_character(),
  description = readr::col_character()
)

# TODO: Write documentation.
#' @title Read a Pfam clans table
#' @description Read a Pfam clans table.
#' @param path Path to file.
#' @return A [tibble::tibble()].
#' @export
read_pfam_clans <- function(path) {
  cols <- cols_pfam_clans
  out <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE)
  out
}
