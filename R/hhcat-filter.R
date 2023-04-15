#' @export
hhcat_filter <- function(hhcat, probability, match_coverage, query_coverage) {
  noself <- dplyr::filter(hhcat, self_match == FALSE)
  tophit <- dplyr::filter(noself, match_rank == min(match_rank), .by = c("query_id", "match_id"))
  probfilt <- dplyr::filter(tophit, prob >= probability)
  covfilt <- dplyr::filter(probfilt, coverage_prop_match >= match_coverage, coverage_prop_query >= query_coverage)
}
