#' @export
combine <- function(list, id) {
  combined <- dplyr::bind_rows(list, .id = id)
  combined
}

#' @export
uncombine <- function(df, id) {
  nested <- tidyr::nest(df, .by = id)
  uncombined <- dplyr::pull(nested, data)
  names(uncombined) <- dplyr::pull(nested, id)
  uncombined
}

#' @export
padlocout_combine <- function(padlocout_list) {
  combine(padlocout_list, "genome.accession")
}

#' @export
padlocout_uncombine <- function(padlocout_comb) {
  uncombine(padlocout_comb, "genome.accession")
}

