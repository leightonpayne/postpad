% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/padlocout-resolve-overlaps.R
\name{padlocout_resolve_overlaps}
\alias{padlocout_resolve_overlaps}
\title{Resolve overlapping systems in a padlocout table.}
\usage{
padlocout_resolve_overlaps(padlocout)
}
\arguments{
\item{padlocout}{A padlocut output table.}
}
\description{
When multiple systems have overlapping components, these are resolved by
first filtering for the system with the most genes, if overlapping systems
have the same amount of genes, an overall score is calculated based on the
domain iE-value and coverages of all genes, so the system with the best
score (i.e. the system predicted with the most confidence) is taken.
}
