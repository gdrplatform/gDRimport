# Load incucyte results from plain text

This functions loads incucyte time-course cell count file

## Usage

``` r
load_results_Incucyte(results_file, headers = gDRutils::get_env_identifiers())
```

## Arguments

- results_file:

  list of strings: file paths to result paths from individual plates

- headers:

  list of headers identified in the manifest

## Value

data.table derived from Incucyte data
