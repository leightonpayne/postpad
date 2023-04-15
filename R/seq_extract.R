#' Extract a DNA/protein sequence from a FASTA file
#'
#' Use the system's `awk` implementation to extract a sequence from a
#' multi-FASTA file based on the identifier in the sequence headers
#' e.g. the identifier for:
#'
#'   ">WP_002564308.1 acyl carrier protein"
#'
#' is:
#'
#'   "WP_002564308.1"
#'
#' @param id The identifier of the sequence for extraction.
#' @param file The path to the FASTA file.
#' @return A character vector with one string per line of the extracted
#' sequence.
#' @export
seq_extract <- function(id, file) {
  command <- "awk"
  args <- paste0("-v seq=", id, " -v RS='>' '$1 == seq {printf RS $0}' ", file)
  seq <- system2(command, args, stdout = TRUE)
  seq
}

#' Parallel implementation of [seq_extract()] using `{furrr}`
#'
#' @param path_db The path to use as a prefix for the `path` column of
#' `seq_extract_index`.
#' @param seq_extract_index A data frame with two columns, `id` and `path`, as
#' created by `make_seq_extract_index`. Where `id` contains the sequence
#' identifiers for extraction and `path` contains the paths to the respective
#' FASTA files (relative to `path_db`).
#' @return A list of character vectors containing sequence information.
#' @export
future_seq_extract <- function(path_db, seq_extract_index) {
  path <- dplyr::pull(seq_extract_index, path)
  path <- purrr::map_chr(.x = path, .f = ~ fs::path_join(c(path_db, .x)))
  id <- dplyr::pull(seq_extract_index, id)
  p <- progressr::progressor(steps = nrow(seq_extract_index))
  seqs <- furrr::future_map2(
    .x = path, .y = id, .f = ~ {p(); seq_extract(.y, .x)}
  )
  seqs
}

#' Prepare an index table for parallel sequence extraction
#'
#' Basic helper function to pull all of the ID's and paths from a loci table
#' and accompanying index table for use with [future_seq_extract()].
#'
#' @param loci_tbl A loci table, as prepared with [build_loci()] (and probably
#' post-processed with [gff_separate_attributes()].
#' @param index An index table with at least a column `genome.accession`, with
#' the names of the genomes that appear in the `loci_tbl` anda column
#' with the respective paths to each genome FASTA file.
#' @param col_id The column in the `loci_tbl` which contains the sequence ID's
#' of interest (in most cases this is probably the column `Name`, which can be
#' pulled out of the column `attributes`, with [gff_separate_attributes()]).
#' @param col_path The column in the `index` table which contains the paths to
#' the FASTA files for each genome.
#' @return A data frame with two columns, `id` and `path`.
#' @export
make_seq_extract_index <- function(loci_tbl, index, col_id, col_path) {
  # Unnecessary
  # # Spread the FAA paths to all proteins in the loci, if they're not already
  # spread <- dplyr::mutate(
  #   loci_tbl,
  #   path_faa = max(path_faa, na.rm = TRUE),
  #   .by = c("genome.accession", "seqid", "system", "system.number")
  # )
  # Filter for rows that came from the GFF file, not the padlocout results.
  filtered <- dplyr::filter(loci_tbl, is.na(target.name))
  selected <- dplyr::select(filtered, genome.accession, col_id)
  indexed <- dplyr::left_join(selected, index, by = dplyr::join_by("genome.accession"))
  selected <- dplyr::select(indexed, col_id, col_path)
  renamed <- dplyr::rename(selected, id = col_id, path = col_path)
  renamed
}


