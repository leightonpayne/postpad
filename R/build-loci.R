#' Build dummy loci
#' @export
build_loci_lazy <- function(padlocout, distance) {
  padlocout_grouped <- dplyr::group_by(
    padlocout,
    genome.accession, seqid, system, system.number
  )
  padlocout_expanded <- tidyr::complete(
    padlocout_grouped,
    relative.position =
      (min(relative.position) - distance):(max(relative.position) + distance)
  )
  padlocout_ungrouped <- dplyr::ungroup(padlocout_expanded)
  padlocout_ungrouped
}

#' test
#' @export
build_loci_test <- function(padlocout, index, path_db, distance) {

  padlocout_grouped <- dplyr::group_by(
    padlocout,
    genome.accession, seqid, system, system.number
  )

  padlocout_expanded <- tidyr::complete(
    padlocout_grouped,
    relative.position =
      (min(relative.position) - distance):(max(relative.position) + distance)
  )

  padlocout_ungrouped <- dplyr::ungroup(padlocout_expanded)

  padlocout_index <- dplyr::left_join(
    padlocout_ungrouped, index,
    by = dplyr::join_by("genome.accession")
  )

  padlocout_nested <- tidyr::nest(
    padlocout_index,
    .by = c("genome.accession", "path_gff")
  )

  nested_data <- dplyr::pull(padlocout_nested, data)

  p <- progressr::progressor(along = nested_data)

  nested_data_w_loci <- purrr::imap(.x = nested_data, .f = function(x, idx) {

    p()

    gff_path <- fs::path_join(c(path_db, dplyr::pull(padlocout_nested[idx,], path_gff)))
    gff <- read_gff(gff_path)
    gff_cds <- dplyr::filter(gff, type == "CDS")
    gff_cds_pos <- gff_mutate_relative_position(gff_cds)

    data_prepped <- dplyr::select(x, -c("start", "end", "strand"))
    joined <- dplyr::left_join(data_prepped, gff_cds_pos, by = dplyr::join_by(seqid, relative.position))
    joined

  })

  padlocout_nested[["data"]] <- nested_data_w_loci
  out <- tidyr::unnest(padlocout_nested, tidyr::everything())
  out

}

#' Build loci
#' @export
build_loci <- function(padlocout, index, path_db, distance) {
  padlocout_loci_limits <- padlocout_mutate_locus_limits(padlocout, distance)
  padlocout_index <- dplyr::left_join(padlocout_loci_limits, index, by = dplyr::join_by("genome.accession"))
  padlocout_nested <- tidyr::nest(padlocout_index, .by = c("genome.accession", "path_gff"))
  nested_data <- dplyr::pull(padlocout_nested, data)

  # p <- progressr::progressor(along = nested_data)

  nested_data_w_loci <- purrr::imap(.x = nested_data, .f = function(x, idx) {

    # p()

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

  }, .progress = TRUE)

  padlocout_nested[["data"]] <- nested_data_w_loci
  out <- tidyr::unnest(padlocout_nested, tidyr::everything())
  out

}

#' Build loci, removing pseudogenes in the process
#' @export
build_loci_wo_pseudos <- function(padlocout, index, path_db, distance) {
  padlocout_loci_limits <- padlocout_mutate_locus_limits(padlocout, distance)
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

      gff_filtered <- dplyr::filter(gff_cds_pos, seqid == cur_seqid)

      joined <- dplyr::left_join(gff_filtered, mutated, by = dplyr::join_by(seqid, relative.position, start, end, strand))
      joined_attr <- gff_separate_attributes(joined)
      # If a non-broken locus came along for the ride with a broken one in the
      # same genome, make sure it doesn't break the pipeline.
      # This may also be fixed by using a different attribute to identify
      # pseudogenes, but can't use string::str_detect(protein_id, "^pseudo_sub")
      # because it won't match those pseudogenes that didn't get substituted.
      if ( ! "pseudo" %in% names(joined_attr) ) {
        joined_attr <- dplyr::mutate(joined_attr, pseudo = NA)
      }
      joined_no_pseudo <- dplyr::filter(joined_attr, is.na(pseudo) | !is.na(target.name))
      joined_renamed <- dplyr::rename(joined_no_pseudo, relative.position_w_pseudo = relative.position)
      joined_reassigned_pos <- gff_mutate_relative_position(joined_renamed)
      joined_reassigned_renamed <- dplyr::rename(
        joined_reassigned_pos, contig.end_w_pseudos = contig.end
      )
      joined_reassigned_ends <- dplyr::mutate(
        joined_reassigned_renamed, contig.end = max(relative.position), .by = "seqid"
      )

      new_locus.min <- dplyr::filter(joined_reassigned_ends, !is.na(target.name)) %>% dplyr::pull(relative.position) %>% min() - distance
      new_locus.max <- dplyr::filter(joined_reassigned_ends, !is.na(target.name)) %>% dplyr::pull(relative.position) %>% max() + distance

      joined_filt <- dplyr::filter(joined_reassigned_ends, dplyr::between(relative.position, new_locus.min, new_locus.max))

      out <- dplyr::select(joined_filt, !seqid)
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


#' Reduce loci
#' @export
reduce_loci <- function(loci, distance) {
  loci_limits_red <- dplyr::mutate(
    loci,
    locus.min = locus.min + distance,
    locus.max = locus.max - distance,
    .by = c("genome.accession", "seqid", "system", "system.number")
  )
  loci_red <- dplyr::filter(
    loci_limits_red,
    dplyr::between(relative.position, locus.min, locus.max),
    .by = c(genome.accession, seqid, system, system.number)
  )
  loci_red
}


loci_remove_pseudos <- function(loci) {

  loci_pseudos <- loci_mutate_pseudo_info(loci)

  loci_have_pseudos <- dplyr::mutate(
    loci_pseudos,
    locus_has_pseudos = dplyr::if_else(
      any(is_adjacent & is_pseudo) | any(is_missing),
      TRUE, FALSE
    ),
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  affected_loci <- dplyr::filter(loci_have_pseudos, locus_has_pseudos == TRUE)
  affected_distinct <- dplyr::distinct(
    affected_loci,
    genome.accession, seqid, system, system.number
  )

  loci_w_pseudos_removed <- dplyr::filter(
    loci_pseudos,
    !is_adjacent | is_adjacent & !is_pseudo | is_adjacent & !is_missing
  )

}

#' @export
loci_identify_pseudos <- function(loci) {

  loci_pseudos <- loci_mutate_pseudo_info(loci)

  loci_have_pseudos <- dplyr::mutate(
    loci_pseudos,
    locus_has_pseudos = dplyr::if_else(
      any(is_adjacent & is_pseudo) | any(is_missing),
      TRUE, FALSE
    ),
    .by = c("genome.accession", "seqid", "system", "system.number")
  )

  affected_loci <- dplyr::filter(loci_have_pseudos, locus_has_pseudos == TRUE)
  affected_distinct <- dplyr::distinct(
    affected_loci,
    genome.accession, seqid, system, system.number
  )
  affected_distinct
}
