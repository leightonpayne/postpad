# These functions are deprecated in favor of tidyverse functions that already
# exist and work a lot more efficiently, but I didn't know existed at the time.
# They are kept here in case I need to refer back to them for some reason.

#' Create a new column based on [names()]
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated as the same functionality can be achieved by
#' using the `.id` argument of [dplyr::bind_rows()].
#' `mutate_namecol()` creates a new column for each data frame in a list,
#' derived from the name of the data frame.
#' @keywords internal
#' @param df_list A [list()] of [data.frame()]s.
#' @param colname The name of the new column.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Pass additional arguments to
#' [dplyr::mutate()] to create, modify, and delete other columns and control
#' where the columns should appear (i.e. `.before` and `.after`).
#' @return A [list()] of [data.frame()]s.
mutate_namecol <- function(df_list, colname, ...) {
  checkmate::check_list(df_list, types = "data.frame", names = "named")
  p <- progressr::progressor(along = df_list)
  out <- purrr::imap(
    .x = df_list,
    .f = function(x, idx, ...) {
      p()
      mutated <- dplyr::mutate(x, "{colname}" := idx, ...)
      mutated
    },
    ...
  )
  out
}

#' Collapse a PADLOC output table to one row per system
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated as the same functionality can be achieved by
#' using [tidyr::nest()].
#' Collapse a PADLOC output table to one row per system.
#' @param padlocout A PADLOC output table (as read-in by [read_padlocout()]).
#' @param select Optionally, a selection of columns to
#' @param drop Relevant when `select` is specified. If `drop = FALSE` (the
#' default), the output table retains only the columns specified. If
#' `drop = TRUE`, column selection is inversed. Selection occurs before
#' collapsing, so dropping uneeded columns may speed up processing.
#' @return A [tibble::tibble()].
collapse_padlocout <- function(padlocout, select = NULL, drop = FALSE) {

  if (!rlang::is_true(checkmate::check_logical(drop))) {
    cli::cli_abort('Invalid value for argument: {.var drop}. {chk_drop}')
  }

  if (is.null(select) && !rlang::is_false(drop)) {
    cli::cli_abort("Can't supply {.var drop} without {.var select}")
  }

  columns <- names(padlocout)
  if (drop == TRUE) select <- setdiff(columns, select)
  to_collapse <- setdiff(select, c("genome.accession", "seqid", "system.number", "system"))

  if (!is.null(select)) {
    selected <- dplyr::select(padlocout, dplyr::all_of(select))
  } else {
    selected <- padlocout
  }

  # Breaks if any of the following were dropped
  grouped <- dplyr::group_by(selected, seqid, system.number)
  collapsed <- dplyr::mutate_at(grouped, to_collapse, ~ paste0(., collapse = ";;"))
  distinct <- dplyr::distinct(collapsed, seqid, system.number, .keep_all = TRUE)
  distinct
}

#' Expand a collapsed PADLOC output table
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated as the same functionality can be achieved by
#' using [tidyr::unnest()].
#' @param collapsed_padlocout A collapsed PADLOC output table
#' @export
expand_padlocout <- function(collapsed_padlocout) {
  expanded <- tidyr::separate_longer_delim(
    collapsed_padlocout,
    dplyr::everything(),
    ";;"
  )
  expanded
}

#' Combine and uncombine a list of padlocout tables
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' This function was deprecated as the same functionality can be achieved by
#' using the `.id` argument of [dplyr::bind_rows()].
#' Combining and uncombining padlocout tables changes their form from a list of
#' data frames to a data frame and vice-versa.
#'
#' Combining padlocout tables can be useful for several reasons:
#'
#'   * Multiple padlocout tables can be read using [multi_read_padlocout()],
#'   combined with [padlocout_combine()], and written to a single file with
#'   [write_padlocout()]).
#'   * Storing multiple padlocout tables as a single data frame is more memory
#'   efficient than as a list of data frames.
#'   * Performing operations on all padlocout tables at once, for example
#'   summarising defence system counts, distribution, etc.
#'   * If there are multiple padlocout tables for a single genome, combining
#'   them with [padlocout_combine()] and then uncombining them again with
#'   [padlocout_uncombine()] will group them into a single list element.
#'
#' Returning padlocout tables to an uncombined state can also be useful for:
#'
#'   * Performing operations on each padlocout table individually with the
#'   [purrr::map()] or [base::apply()] family of functions.
#'
#' @param padlocout_list A list of padlocout tables (as read in by
#' [multi_read_padlocout()]).
#' @return A combined padlocout table, introducing a new row `genome_accession`,
#' which is derived from the names of the padlocout tables in `padlocout_list`.
padlocout_combine <- function(padlocout_list) {
  to_combine <- purrr::map(
    .x = cli::cli_progress_along(padlocout_list, name = "Combining PADLOC output"),
    .f = function(i) {
      # Check if there's a column for genome accession, and if not make one
      if (! "genome.accession" %in% names(padlocout_list[[i]])) {
        file_name <- names(padlocout_list[i])
        x <- dplyr::mutate(padlocout_list[[i]], "genome.accession" = file_name)
        x <- dplyr::select(x, "genome.accession", dplyr::everything())
        x
      } else {
        padlocout_list[[i]]
      }
    }
  )
  combined <- dplyr::bind_rows(to_combine)
  combined
}

#' Uncombine a combined padlocout table
#' @rdname padlocout_combine
#' @param combined_padlocout A combined padlocout table as constructed with
#' [padlocout_combine()].
padlocout_uncombine <- function(combined_padlocout) {
  dropped_accession <- dplyr::select(combined_padlocout, -genome.accession)
  uncombined <- split(dropped_accession, combined_padlocout$genome.accession)
  uncombined
}

#' Read in a fasta file
read_fasta_file <- function(file_path) {
  # Open the file
  file_con <- file(file_path, "r")

  # Initialize variables
  current_header <- ""
  current_seq <- ""

  # Initialize list to store sequences
  sequences <- list()

  # Loop over each line of the file
  while (length(current_line <- readLines(file_con, n = 1)) > 0) {
    # Check if the line is a header line
    if (startsWith(current_line, ">")) {
      # Store the previous sequence (if any)
      if (nchar(current_seq) > 0) {
        sequences[[current_header]] <- current_seq
      }

      # Set the new header and sequence variables
      current_header <- sub("^>(\\S+).*", "\\1", current_line)
      current_seq <- ""
    } else {
      # Append the current line to the current sequence
      current_seq <- paste0(current_seq, current_line)
    }
  }

  # Store the last sequence (if any)
  if (nchar(current_seq) > 0) {
    sequences[[current_header]] <- current_seq
  }

  # Close the file
  close(file_con)

  # Return the list of sequences
  return(sequences)
}
