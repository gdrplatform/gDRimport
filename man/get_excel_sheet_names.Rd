% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_files.R
\name{get_excel_sheet_names}
\alias{get_excel_sheet_names}
\title{get Excel sheets names for a charvec of files
for non-Excel files return 0}
\usage{
get_excel_sheet_names(fls)
}
\arguments{
\item{fls}{charvec with file pathsa}
}
\value{
list with one element per file with
sheet names or 0 (for non-Excel file)
}
\description{
get Excel sheets names for a charvec of files
for non-Excel files return 0
}
\keyword{load_files}
