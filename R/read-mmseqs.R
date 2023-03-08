cols_mmseqs_cluster_tsv <- readr::cols(
  clu_representative = readr::col_character(),
  clu_member         = readr::col_character()
)

#' Read a mmseqs cluster TSV
#' @param path A character vector of full path names; the default corresponds
#' to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#' performed. Missing values will be ignored. Elements with a marked encoding
#' will be converted to the native encoding (and if that fails, considered
#' non-existent).
#' @export
read_mmseqs_cluster_tsv <- function(path) {
  cols <- cols_mmseqs_cluster_tsv
  out <- readr::read_tsv(path, col_names = names(cols$cols), col_types = cols,
                         progress = FALSE, comment = "#")
  out
}
