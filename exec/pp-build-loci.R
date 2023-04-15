#!/usr/bin/env Rscript

spec = matrix(c(
  "input"     , "i", 1, "character", "Path to master padloc output file.",
  "output"    , "o", 1, "character", "Path to output RDS file where loci will be written.",
  "seqdb"     , "s", 1, "character", "Path to the sequence database containing GFF files.",
  "index"     , "x", 1, "character", "Path to the index file containing file paths relative to the sequence database path.",
  "distance"  , "d", 1, "double"   , "Number of genes to gather into locus.",
  "cpu"       , "c", 1, "double"   , "Number of cpus to split job across.",
  "force"     , "f", 0, "logical"  , "Force overwritting of output.",
  "help"      , "h", 0, "logical"  , "Print this help message."
), byrow = TRUE, ncol = 5)
opt <- getopt::getopt(spec)

if (is.null(opt$force)) opt$force <- FALSE
if (is.null(opt$help)) opt$help <- FALSE

if (opt$help == TRUE) {
  usage <- gsub("\f$" , "", gsub("\n", "\f", getopt::getopt(spec, usage = TRUE, command = "pp-build-loci.R")))
  cli::cli_text(usage)
  if (!interactive()) q(status = 0)
}

collection <- checkmate::makeAssertCollection()
checkmate::assert_file_exists(opt$input, access = "rw", .var.name = "-i|--input", add = collection)
checkmate::assert_path_for_output(opt$output, overwrite = opt$force, .var.name = "-o|--output", add = collection)
if (!collection$isEmpty()) {
  options(rlang_backtrace_on_error = "none")
  cli::cli_abort(c("Invalid arguments.", collection$getMessages()))
}

# Make all calls to progressr print a progress bar.
progressr::handlers(global = TRUE)
progressr::handlers("cli")

# Set future plan
future::plan(future.callr::callr, workers = opt$cpu)

index <- postpad::read_db_index(opt$index)
padlocout_master <- postpad::read_padlocout_master(opt$input)

loci <- postpad::build_loci(padlocout_master, index, opt$seqdb, opt$distance)

readr::write_rds(loci, opt$output)
