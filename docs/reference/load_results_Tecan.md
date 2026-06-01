# Load tecan results from xlsx

This functions loads and checks the results file

## Usage

``` r
load_results_Tecan(results_file, headers = gDRutils::get_env_identifiers())
```

## Arguments

- results_file:

  string, file path to a result file

- headers:

  list of headers identified in the manifest

## Value

data.table derived from Tecan data
