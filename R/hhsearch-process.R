#' @export
hhcat_process <- function(hhcat) {
  # Re-order columns
  hhcat_reordered <- dplyr::select(hhcat, query_id, match_id, dplyr::everything())
  hhcat_sepalign <- tidyr::separate_wider_delim(
    hhcat_reordered,
    cols = dplyr::all_of(c("alignment_query", "alignment_match")),
    delim = "-",
    names = c("start", "end"),
    names_sep = "_"
  )
  hhcat_mleng <- dplyr::mutate(
    hhcat_sepalign,
    match_hmm_length = gsub("\\(|\\)", "", match_hmm_length)
  )
  hhcat_types <- dplyr::mutate(
    hhcat_mleng,
    dplyr::across(
      .cols = c(
        alignment_query_start,
        alignment_query_end,
        alignment_match_start,
        alignment_match_end,
        match_hmm_length,
        ),
      .fns = ~ as.numeric(.x)
      )
    )
  hhcat_self <- dplyr::mutate(
    hhcat_types,
    self_match = dplyr::if_else(match_id == query_id, TRUE, FALSE),
    .by = "query_id"
  )
  hhcat_cov <- dplyr::mutate(
    hhcat_self,
    query_hmm_coverage = alignment_query_end - (alignment_query_start - 1),
    match_hmm_coverage = alignment_match_end - (alignment_match_start - 1),
    coverage_prop_match = match_hmm_coverage / match_hmm_length * 100,
    coverage_prop_query = query_hmm_coverage / query_hmm_length * 100
  )
  hhcat_cov
}
