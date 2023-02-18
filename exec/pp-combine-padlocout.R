#!/usr/bin/env Rscript

spec = matrix(c(
  "input"     , "i", 1, "character", "Path to input directory containing '*_padloc.csv' files.",
  "output"    , "o", 1, "character", "Path to output file where the combined padloc output will be written.",
  "recursive" , "r", 0, "logical"  , "Is this a recursive combination?, i.e. is this a combination of already combined outputs?",
  "force"     , "f", 0, "logical"  , "Force overwritting of output.",
  "help"      , "h", 0, "logical"  , "Print this help message."
), byrow = TRUE, ncol = 5)
opt <- getopt::getopt(spec)

if (is.null(opt$recursive)) opt$recursive <- FALSE
if (is.null(opt$force)) opt$force <- FALSE
if (is.null(opt$help)) opt$help <- FALSE

if (opt$help == TRUE) {
  usage <- gsub("\f$" , "", gsub("\n", "\f", getopt::getopt(spec, usage = TRUE, command = "pp-combine-padlocout.R")))
  cli::cli_text(usage)
  if (!interactive()) q(status = 0)
}

collection <- checkmate::makeAssertCollection()
checkmate::assert_directory_exists(opt$input, access = "rw", .var.name = "-i|--input", add = collection)
checkmate::assert_path_for_output(opt$output, overwrite = opt$force, .var.name = "-o|--output", add = collection)
if (!collection$isEmpty()) {
  options(rlang_backtrace_on_error = "none")
  cli::cli_abort(c("Invalid arguments.", collection$getMessages()))
}

if (opt$recursive == TRUE) {
  padlocout_list <- postpad::multi_read_padlocout_master(opt$input)
} else {
  padlocout_list <- postpad::multi_read_padlocout(opt$input)
}
padlocout_combined <- postpad::combine_padlocout(padlocout_list)
readr::write_csv(padlocout_combined, opt$output)
