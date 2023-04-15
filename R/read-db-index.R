cols_db_index <- readr::cols(
  chunk            = readr::col_character(),
  genome.accession = readr::col_character(),
  path_faa         = readr::col_character(),
  path_gff         = readr::col_character()
)


#' Read a RefSeq database index file
#'
#' A RefSeq database has RefSeq genome FASTA and GFF files divided into
#' directory 'chunks'. The index file for the database should be a TSV file with
#' a header and one row per genome, with columns listing the directory/chunk
#' where the respective genome files are located i.e. `chunk`, the accession
#' number of the genome i.e. `genome.accession`, i.e. how the genome is
#' identified in a `padlocout_master` table, the path to the respective genome
#' FASTA file i.e. `path_faa`, relative to the base directory of the RefSeq
#' database, and the path to the respective genome GFF file i.e. `path_gff`,
#' relative to the base directory of the RefSeq database.
#'
#' @param path The path to the RefSeq database index.
#' @return A [tibble::tibble()].
#' @export
read_db_index <- function(path) {
  cols <- cols_db_index
  out <- readr::read_tsv(
    path,
    col_names = names(cols$cols),
    col_types = cols,
    progress = FALSE,
    skip = 1
  )
  out
}
