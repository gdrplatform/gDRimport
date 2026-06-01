# Load EnVision results from xlsx

This functions loads and checks the results file(s)

## Usage

``` r
load_results_EnVision(results_file, headers = gDRutils::get_env_identifiers())
```

## Arguments

- results_file:

  character vector containing file path(s) to results file(s)

- headers:

  list of headers identified in the manifest

## Value

data.table with results data
