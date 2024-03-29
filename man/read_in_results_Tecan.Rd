% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_files.R
\name{read_in_results_Tecan}
\alias{read_in_results_Tecan}
\title{read in Tecan data}
\usage{
read_in_results_Tecan(results_file, results_sheets, headers)
}
\arguments{
\item{results_file}{string, file path to a result file}

\item{results_sheets}{template sheet names}

\item{headers}{list of headers identified in the manifest}
}
\value{
data.table derived from Tecan data
}
\description{
read in Tecan data
}
\keyword{load_files}
