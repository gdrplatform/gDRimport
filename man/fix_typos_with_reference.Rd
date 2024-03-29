% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correction.R
\name{fix_typos_with_reference}
\alias{fix_typos_with_reference}
\title{Fix typos using reference data}
\usage{
fix_typos_with_reference(
  data,
  ref,
  method = c("exact", "grepl", "adist"),
  fix_underscores = FALSE
)
}
\arguments{
\item{data}{list of charvec(s) or charvec with data}

\item{ref}{charvec with reference data}

\item{method}{charvec type of the method to be used
'exact' is used to find  identical entries from 'ref' in the
data (after corrections and uppercase'ing)
'grepl' is used to find entries from 'ref' that might be
somehow pre- or post- fixed}

\item{fix_underscores}{logical flag fix the issues with underscores in data identfiers?}
}
\value{
list or charvec with corrected data
}
\description{
Fix typos using reference data
Evaluate given list of ids and try to update them
}
\keyword{correction_exception}
