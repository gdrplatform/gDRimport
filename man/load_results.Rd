% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_files.R
\name{load_results}
\alias{load_results}
\title{Load results}
\usage{
load_results(
  df_results_files,
  instrument = "EnVision",
  headers = gDRutils::get_env_identifiers()
)
}
\arguments{
\item{df_results_files}{data.table, with datapaths and names of results file(s)
or character with file path of results file(s)}

\item{instrument}{character}

\item{headers}{list of headers identified in the manifest file}
}
\value{
data.table with results' data
}
\description{
This functions loads and checks the results file(s)
}
\examples{
 td <- get_test_data()
 r_df <- load_results(result_path(td))

}
\keyword{load_files}
