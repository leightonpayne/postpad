#' Build loci
#' @export
build_loci <- function(padlocout, index, path_db) {
  padlocout_loci_limits <- padlocout_mutate_locus_limits(padlocout, 3)
  padlocout_index <- dplyr::left_join(padlocout_loci_limits, index, by = dplyr::join_by("genome.accession"))
  padlocout_nested <- tidyr::nest(padlocout_index, .by = c("genome.accession", "path_gff"))
  nested_data <- dplyr::pull(padlocout_nested, data)

  p <- progressr::progressor(along = nested_data)

  nested_data_w_loci <- purrr::imap(.x = nested_data, .f = function(x, idx) {

    p()

    gff_path <- fs::path_join(c(path_db, dplyr::pull(padlocout_nested[idx,], path_gff)))
    gff <- read_gff(gff_path)
    gff_cds <- dplyr::filter(gff, type == "CDS")
    gff_cds_pos <- gff_mutate_relative_position(gff_cds)

    systems_nested <- tidyr::nest(x, .by = c(seqid, system, system.number, locus.min, locus.max))
    systems_nested_data <- dplyr::pull(systems_nested, data)

    systems_nested_data_new <- purrr::imap(.x = systems_nested_data, .f = function(x, idx) {

      cur_seqid <- dplyr::pull(systems_nested[idx,], seqid)
      mutated <- dplyr::mutate(x, seqid = cur_seqid)

      cur_locus.min <- dplyr::pull(systems_nested[idx,], locus.min)
      cur_locus.max <- dplyr::pull(systems_nested[idx,], locus.max)
      gff_filtered <- dplyr::filter(gff_cds_pos, seqid == cur_seqid & dplyr::between(relative.position, cur_locus.min, cur_locus.max))

      joined <- dplyr::left_join(gff_filtered, mutated, by = dplyr::join_by(seqid, relative.position, start, end, strand))

      out <- dplyr::select(joined, !seqid)
      out

    })

    systems_nested[["data"]] <- systems_nested_data_new
    out <- tidyr::unnest(systems_nested, tidyr::everything())
    out

  })

  padlocout_nested[["data"]] <- nested_data_w_loci
  out <- tidyr::unnest(padlocout_nested, tidyr::everything())
  out

}
