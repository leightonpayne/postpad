#' Resolve overlapping systems in a padlocout table.
#'
#' When multiple systems have overlapping components, these are resolved by
#' first filtering for the system with the most genes, if overlapping systems
#' have the same amount of genes, an overall score is calculated based on the
#' domain iE-value and coverages of all genes, so the system with the best
#' score (i.e. the system predicted with the most confidence) is taken.
#'
#' @param padlocout A padlocut output table.
#'
#' @export
padlocout_resolve_overlaps <- function(padlocout) {

  # Add a column for the number of genes in a system
  gene_counts <- dplyr::mutate(
    padlocout,
    system_gene_count = dplyr::n(),
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  # Filter genes, by position, for genes belonging to the system with the most
  # genes.
  most_genes <- dplyr::filter(
    gene_counts,
    system_gene_count == max(system_gene_count),
    .by = c("genome.accession", "seqid", "relative.position")
  )

  # Drop any left over genes from partially removed systems.
  most_genes_clean <- dplyr::filter(
    most_genes,
    dplyr::n() == system_gene_count,
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  # Estimate lowest e-value, for hits where e-value = 0.
  # I'm pretty sure the lowest e-value reported by HMMER is 1e-320, but I don't
  # remember where I read this.
  correct_evals <- dplyr::mutate(
    most_genes_clean,
    domain.iE.value.est = ifelse(domain.iE.value == 0, 1e-320, domain.iE.value)
  )

  # Calculate a system score.
  scored <- dplyr::mutate(
    correct_evals,
    system_score = sum(-log10(domain.iE.value.est) * target.coverage * hmm.coverage),
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  # Filter genes, by position, for genes belonging to the system with best
  # score.
  best_score <- dplyr::filter(
    scored,
    system_score == max(system_score),
    .by = c("genome.accession", "seqid", "relative.position")
  )

  # Drop any left over genes from partially removed systems.
  best_score_clean <- dplyr::filter(
    best_score,
    dplyr::n() == system_gene_count,
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  best_score_clean

}
