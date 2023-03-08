combine <- function(list, id) {
  combined <- dplyr::bind_rows(list, .id = id)
  combined
}

uncombine <- function(df, id) {
  nested <- tidyr::nest(df, .by = id)
  uncombined <- dplyr::pull(nested, data)
  names(uncombined) <- dplyr::pull(nested, id)
  uncombined
}

padlocout_combine <- function(padlocout_list) {
  combine(padlocout_list, "genome.accession")
}

padlocout_uncombine <- function(padlocout_comb) {
  uncombine(padlocout_comb, "genome.accession")
}

