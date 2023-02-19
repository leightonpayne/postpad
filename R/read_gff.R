cols_gff <- readr::cols(
  seqid      = readr::col_character(),
  source     = readr::col_character(),
  type       = readr::col_character(),
  start      = readr::col_double(),
  end        = readr::col_double(),
  score      = readr::col_character(),
  strand     = readr::col_character(),
  phase      = readr::col_character(),
  attributes = readr::col_character()
)

read_gff <- function(path) {
  cols <- cols_gff
  out <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE, comment = "#")
  out
}

#' Read multiple GFF files
#' @param path A character vector of full path names; the default corresponds
#' to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#' performed. Missing values will be ignored. Elements with a marked encoding
#' will be converted to the native encoding (and if that fails, considered
#' non-existent).
#' @return A [list()], where each element is a [tibble::tibble()] holding
#' the contents of the file.
#' @export
multi_read_gff <- function(path) {
  out <- multi_read(path = path, func = read_gff, pattern = "*_genomic.gff", name = "Reading GFF files")
  out
}

