

# padlocout_nested <- tidyr::nest(padlocout_master, .by = c("genome.accession", "seqid", "system", "system.number"))


#' Create a column for total separation between the genes of a system
#' NA.
#' @export
padlocout_mutate_separation <- function(padlocout) {
  grouped <- dplyr::group_by(padlocout, seqid, system, system.number)
  limits <- dplyr::mutate(grouped, max = max(relative.position), min = min(relative.position), genes = dplyr::n())
  separation <- dplyr::mutate(limits, separation = max + 1 - min - genes)
  ungrouped <- dplyr::ungroup(separation)
  out <- dplyr::select(ungrouped, -c(max, min, genes))
  out
}

#' Create columns for minimum/maximum locus limits
#' @export
padlocout_mutate_locus_limits <- function(padlocout, distance) {
  a <- dplyr::group_by(padlocout, seqid, system.number)
  b <- dplyr::mutate(a, locus.min = min(relative.position) - distance,
              locus.max = max(relative.position) + distance)
  c <- dplyr::ungroup(b)
  c
}

#' Create a column for relative position
#' @export
gff_mutate_relative_position <- function(gff, name = "relative.position") {
  arranged <- dplyr::arrange(gff, seqid, start)
  grouped <- dplyr::group_by(arranged, seqid, type)
  mutated <- dplyr::mutate(grouped, {{ name }} := dplyr::row_number())
  ungrouped <- dplyr::ungroup(mutated)
  ungrouped
}

padlocout_discard <- c(
  "target.name",
  "hmm.accession",
  "hmm.name",
  "protein.name",
  "full.seq.E.value",
  "domain.iE.value",
  "target.coverage",
  "hmm.coverage",
  "start",
  "end",
  "strand",
  "target.description",
  "relative.position",
  "contig.end",
  "all.domains",
  "best.hits"
  )


#' @export
gff_filter_locus <- function(padlocout, gff, distance) {
  padlocout_loci <- calculate_loci(padlocout, distance)
  padlocout_loci_unq <- dplyr::distinct(padlocout_loci, seqid, system.number, system, locus.min, locus.max, .keep_all = TRUE)
  padlocout_loci_sel <- dplyr::select(padlocout_loci_unq, -dplyr::all_of(padlocout_discard))
  gff_filtered <- purrr::pmap(
    .l = padlocout_loci_sel,
    .f = function(...) {
      # A data frame is a list of columns, so map() functions work on a data frame
      # column-wise, but we can use (...) to pass all columns in parallel, w/
      # pmap() and reconstruct a data frame with a single row.
      cur_row <- tibble::tibble(...)
      out <- dplyr::filter(
        gff,
        seqid == cur_row$seqid &
          dplyr::between(relative.position, cur_row$locus.min, cur_row$locus.max))
      out
    }
  )
  names(gff_filtered) <- paste0(padlocout_loci_unq$system, "_", padlocout_loci_unq$system.number)
  gff_filtered
}

#' @export
gff_filter_locus <- function(padlocout, gff, distance) {
  padlocout_loci <- padlocout_mutate_locus_limits(padlocout, distance)
  padlocout_loci_nested <- tidyr::nest(padlocout_loci, .by = c("seqid", "system", "system.number"))
  # padlocout_loci_unq <- dplyr::distinct(padlocout_loci, seqid, system.number, system, locus.min, locus.max, .keep_all = TRUE)
  # padlocout_loci_sel <- dplyr::select(padlocout_loci_unq, -dplyr::all_of(padlocout_discard))
  nested_data <- dplyr::pull(padlocout_loci_nested, data)
  nested_data_w_gff <- purrr::map(
    .x = nested_data,
    .f = function(x) {
      out <- dplyr::left_join(x, gff, by = dplyr::join_by("seqid", "relative.position"))
      out <- dplyr::filter(gff, seqid == cur_row$seqid & dplyr::between(relative.position, cur_row$locus.min, cur_row$locus.max))
      out
    }
  )
  names(gff_filtered) <- paste0(padlocout_loci_unq$system, "_", padlocout_loci_unq$system.number)
  gff_filtered
}


# Join a PADLOC output table to a GFF table
#' @export
gff_join_padlocout <- function(gff, padlocout) {
  out <- dplyr::left_join(gff, padlocout, by = dplyr::join_by(seqid, start, end, strand, relative.position))
  out
}









