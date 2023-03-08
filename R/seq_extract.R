#' Extract a DNA/protein sequence from a FASTA file
#' @param id The identifier of the sequence for extraction.
#' @param file The path to the FASTA file.
#' @return A character vector with one string per line of the extracted sequence.
#' @export
seq_extract <- function(id, file) {
  command <- "awk"
  args <- paste0("-v seq=", id, " -v RS='>' '$1 == seq {printf RS $0}' ", file)
  seq <- system2(command, args, stdout = TRUE)
  seq
}

#' export
future_seq_extract <- function(proteins_index, path_db) {
  path <- purrr::pluck(proteins_index, "path_faa")
  path <- purrr::map_chr(.x = path, .f = ~ fs::path_join(c(path_db, .x)))
  id <- purrr::pluck(proteins_index, "Name")
  p <- progressr::progressor(steps = nrow(proteins_index))
  seqs <- furrr::future_map2(
    .x = path, .y = id, .f = ~ {p(); seq_extract(.y, .x)}
  )
  seqs
}
