#' @export
loci_mutate_pseudo_info <- function(loci) {
  loci_attr <- gff_separate_attributes(loci)
  loci_pseudos <- loci_attr %>%
    dplyr::mutate(
      is_adjacent = dplyr::if_else(is.na(target.name), TRUE, FALSE),
      is_pseudo = dplyr::if_else(stringr::str_detect(Name, "^pseudo_sub"), TRUE, FALSE),
      is_missing = dplyr::if_else(is.na(Name), TRUE, FALSE)
    )
  loci_pseudos
}

#' @export
loci_summarise_pseudos_by_gene <- function(loci_pseudo_info, distance) {
  pseudo_summary_by_gene <- loci_pseudo_info %>%
    summarise_proportion(.by = c("is_adjacent", "is_pseudo", "is_missing")) %>%
    dplyr::mutate(loci_limits = distance) %>%
    dplyr::arrange(is_adjacent, is_pseudo)
  pseudo_summary_by_gene
}

#' @export
loci_summarise_pseudos_by_system <- function(loci_pseudo_info, distance) {
  pseudo_summary_by_system <- loci_pseudo_info %>%
    dplyr::mutate(
      is_adj_pseudo = dplyr::if_else(is_adjacent & is_pseudo, TRUE, FALSE),
      is_adj_missing = dplyr::if_else(is_adjacent & is_missing, TRUE, FALSE)
      ) %>%
    dplyr::mutate(
      has_adj_pseudo = dplyr::if_else(any(is_adj_pseudo, na.rm = TRUE), TRUE, FALSE),
      has_adj_missing = dplyr::if_else(any(is_adj_missing, na.rm = TRUE), TRUE, FALSE),
      .by = c("genome.accession", "seqid", "system", "system.number")
    ) %>%
    tidyr::nest(.by = c("genome.accession", "seqid", "system", "system.number", "has_adj_pseudo", "has_adj_missing")) %>%
    summarise_proportion(.by = c("has_adj_pseudo", "has_adj_missing")) %>%
    dplyr::mutate(loci_limits = distance)
  pseudo_summary_by_system
}

#' @export
loci_summarise_pseudos <- function(loci, distance) {
  loci_pseudo_info <- loci_mutate_pseudo_info(loci)
  loci_pseudo_summary_by_gene <- loci_summarise_pseudos_by_gene(loci_pseudo_info, distance)
  loci_pseudo_summary_by_system <- loci_summarise_pseudos_by_system(loci_pseudo_info, distance)
  list(loci_pseudo_summary_by_gene, loci_pseudo_summary_by_system)
}
