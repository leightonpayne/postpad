gff_list_attributes <- function(gff) {
  keys <- stringr::str_extract_all(gff$attributes, "[^;=]+(?==)")
  keys_unq <- unique(unlist(keys, use.names = FALSE))
  keys_unq
}

gff_separate_attributes <- function(gff, keep = "all") {
  keys <- gff_list_attributes(gff)
  # check <- checkmate::assert_subset(x = keep, choices = keys, empty.ok = TRUE)
  a <- tidyr::separate_longer_delim(gff, cols = attributes, delim = ";")
  b <- tidyr::separate_wider_delim(a, cols = attributes, delim = "=", names = c("key", "value"))
  c <- tidyr::pivot_wider(b, names_from = key, values_from = value)
  if (!"all" %in% keep) {
    d <- dplyr::select(c, -c(keys[!keys %in% keep]))
    d
  } else {
    c
  }
}

# I thought this might be faster than `separate_attributes()`, but it's not.
# select_attributes <- function(gff, attributes) {
#   pattern <- paste0(attributes, "=.*?(?=;)", collapse = "|")
#   out <- dplyr::mutate(
#     gff_cds,
#     attributes = paste0(unlist(stringr::str_extract_all(attributes, pattern)), collapse = ";"),
#     .by = c(seqid, start, end)
#   )
#   out
# }
