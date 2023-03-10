% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-gff.R
\name{multi_read_gff}
\alias{multi_read_gff}
\title{Read multiple GFF files}
\usage{
multi_read_gff(path, namefix = NULL)
}
\arguments{
\item{path}{A character vector of full path names; the default corresponds
to the working directory, \code{\link[=getwd]{getwd()}}. Tilde expansion (see \link{path.expand}) is
performed. Missing values will be ignored. Elements with a marked encoding
will be converted to the native encoding (and if that fails, considered
non-existent).}
}
\value{
A \code{\link[=list]{list()}}, where each element is a \code{\link[tibble:tibble]{tibble::tibble()}} holding
the contents of the file.
}
\description{
Read multiple GFF files
}
