# Load results from EnVision_new (CSV and XLSX)

This functions loads and checks the results file(s) from a new Envision
instrument in the CSV or XLSX format. Supports multiple plates in a
single file or multiple sheets in an Excel file by robustly checking the
file structure.

## Usage

``` r
load_results_EnVision_new(
  results_file,
  headers = gDRutils::get_env_identifiers()
)
```

## Arguments

- results_file:

  character, file path(s) to result file(s)

- headers:

  list of headers identified in the manifest

## Value

data.table with results data
