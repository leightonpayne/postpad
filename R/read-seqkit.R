cols_seqkit_duplicates <- readr::cols(
  dup_n      = readr::col_double(),
  dup_member = readr::col_character()
)

#' Read a seqkit duplicates file
#' @param path A character vector of full path names; the default corresponds
#' to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#' performed. Missing values will be ignored. Elements with a marked encoding
#' will be converted to the native encoding (and if that fails, considered
#' non-existent).
#' @export
read_seqkit_duplicates <- function(path) {
  cols <- cols_seqkit_duplicates
  raw <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE, comment = "#")
  clean <- dplyr::mutate(
    raw,
    dup_representative = stringr::str_extract(dup_member, "^.*?(?=,)"),
    dup_member = stringr::str_split(dup_member, ", "))
  out <- dplyr::select(clean, dup_representative, dup_member, dup_n)
  out
}
