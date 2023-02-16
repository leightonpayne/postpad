combine_padlocout <- function(padlocout_list) {
  extra_col <- purrr::map(
    .x = cli::cli_progress_along(padlocout_list),
    .f = function(i) {
      file_name <- names(padlocout_list[i])
      genome_accession <- stringr::str_remove(file_name, "_protein_padloc.csv")
      x <- dplyr::mutate(padlocout_list[[i]], "genome.accession" = genome_accession)
      x <- dplyr::select(x, "genome.accession", dplyr::everything())
      x
    }
  )
  combined <- dplyr::bind_rows(extra_col)
  combined
}

