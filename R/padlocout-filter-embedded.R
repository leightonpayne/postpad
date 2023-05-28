#' Filter padlocout for systems with embedded genes
#' @param padlocout A padlocout output table
#' @return A [tibble::tibble()]
#' @export
padlocout_filter_embedded <- function(padlocout) {
  unq_sys <- c("genome.accession", "seqid", "system.number", "system")
  padlocout_units <- padlocout %>%
    group_by_position(
      col = relative.position,
      n = 1,
      name = "unit",
      .by = unq_sys
    )
  padlocout_has_embedded <- padlocout_units %>%
    dplyr::mutate(
      has_embedded = dplyr::if_else(max(unit) > 1, TRUE, FALSE),
      .by = unq_sys
    )
  out <- padlocout_has_embedded %>% dplyr::filter(has_embedded == TRUE)
  out
}
