#' @export
hhcat_graph <- function(hhcat) {
  weighted <- dplyr::mutate(hhcat, weights = prob)
  nested <- tidyr::nest(weighted, .by = c(query_id, match_id, weights))
  # paired <- dplyr::mutate(
  #   nested,
  #   pair = paste(sort(c(query_id, match_id)), collapse = ";"),
  #   .by = c("query_id", "match_id")
  #   )
  # distinct <- dplyr::distinct(paired, pair, .keep_all = TRUE)
  graph <- tidygraph::as_tbl_graph(nested, directed = TRUE)
}
