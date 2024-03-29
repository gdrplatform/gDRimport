% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_files.R
\name{load_results_Tecan}
\alias{load_results_Tecan}
\title{Load tecan results from xlsx}
\usage{
load_results_Tecan(results_file, headers = gDRutils::get_env_identifiers())
}
\arguments{
\item{results_file}{string, file path to a result file}

\item{headers}{list of headers identified in the manifest}
}
\value{
data.table derived from Tecan data
}
\description{
This functions loads and checks the results file
}
\keyword{load_files}
