% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_files.R
\name{read_EnVision_delim}
\alias{read_EnVision_delim}
\title{Read EnVision delimited text files}
\usage{
read_EnVision_delim(file, nrows = 10000, seps = c(",", "\\t"))
}
\arguments{
\item{file}{string to path of input file from EnVision scanner}

\item{nrows}{maximum number of file rows to be processed}

\item{seps}{potential field separators of the input file}
}
\value{
a list containing the data table, n_col, n_row, and if is edited
}
\description{
This function reads file from the EnVision Workstation
}
\keyword{load_files}
