#' @include utils.R
NULL

cols_hhcat <- readr::cols(
  match_rank          = readr::col_double(),
  match_id            = readr::col_character(),
  prob                = readr::col_double(),
  evalue              = readr::col_double(),
  pvalue              = readr::col_double(),
  score               = readr::col_double(),
  secondary_structure = readr::col_double(),
  alignmnent_width    = readr::col_double(),
  alignment_query     = readr::col_character(),
  alignment_match     = readr::col_character(),
  match_hmm_length    = readr::col_character(),
  query_id            = readr::col_character(),
  query_hmm_length    = readr::col_double()
)

#' @export
read_hhcat <- function(path) {
  out <- readr::read_tsv(path, col_names = names(cols_hhcat$cols), col_types = cols_hhcat)
  out
}
