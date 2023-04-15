#' @export
loci_tbl_add_locus_structure <- function(loci_tbl) {
  mutated <- dplyr::mutate(
    loci_tbl,
    locus_structure = paste0(clu_representative, collapse = ">>"),
    .by = c(genome.accession, seqid, system, system.number))
  out <- dplyr::arrange(mutated, genome.accession, seqid, system, system.number, relative.position)
  out
}

#' @export
loci_tbl_nest_structure <- function(loci_tbl) {
  loci_tbl_w_structure <- loci_tbl_add_locus_structure(loci_tbl)
  loci_nested <- tidyr::nest(loci_tbl_w_structure, .by = c(genome.accession, seqid, system, system.number, locus_structure))
  loci_nested_count <- dplyr::mutate(loci_nested, locus_structure_n = dplyr::n(), .by = locus_structure)
  loci_stucture_nested <- tidyr::nest(loci_nested_count, .by = c(locus_structure, locus_structure_n))
  out <- dplyr::arrange(loci_stucture_nested, desc(locus_structure_n))
}
