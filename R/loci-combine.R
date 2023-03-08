loci_combine <- function(loci_list) {
  loci_unlisted <- purrr::map(
    .x = cli::cli_progress_along(loci_list, name = "Unlisting loci"),
    .f = function(genome_id) {
      loci_named <- purrr::imap(
        .x = loci_list[[genome_id]],
        .f = function(locus, locus_id) {
          locus_named <- dplyr::mutate(
            loci_list[[genome_id]][[locus_id]],
            locus_name = names(loci_list[[genome_id]][locus_id])
          )
          locus_named
        }
      )
      unlisted <- dplyr::bind_rows(loci_named)
      unlisted
    }
  )
  names(loci_unlisted) <- names(loci_list)
  combined <- padlocout_combine(loci_unlisted)
}
