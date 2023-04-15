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

#' @export
gff_attributes <- c("ID", "Parent", "Dbxref", "Name", "gbkey", "inference", "locus_tag", "product", "protein_id", "transl_table", "Note", "end_range", "partial", "pseudo", "gene", "start_range", "exception", "transl_except")

#' @export
gff_collect_attributes <- function(gff, attributes = gff_attributes) {
  attributes_in_gff <- intersect(names(gff), attributes)
  id_rows <- dplyr::mutate(gff, unq = dplyr::row_number())
  pivoted <- tidyr::pivot_longer(id_rows, cols = attributes_in_gff, names_to = "attribute", values_to = "value")
  nas_removed <- dplyr::filter(pivoted, !is.na(value))
  attributed <- dplyr::mutate(nas_removed, attribute_value = paste0(attribute, "=", value))
  merged_attributes <- dplyr::mutate(attributed, attributes = paste0(attribute_value, collapse = ";"), .by = unq)
  distinct <- dplyr::distinct(merged_attributes, unq, .keep_all = TRUE)
  out <- dplyr::select(distinct, -c(dplyr::any_of(attributes_in_gff), unq, attribute, value, attribute_value))
  out
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
