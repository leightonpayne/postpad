#!/usr/bin/env Rscript

library(cli)
library(getopt)

# set spec
spec <- matrix(c(
  'input'  , 'i', 1, "character", "input"
), byrow = TRUE, ncol = 5)

# process options
opt <- getopt::getopt(spec)

cli::cli_alert_info('{opt$input}')
