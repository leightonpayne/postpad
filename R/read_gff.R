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
