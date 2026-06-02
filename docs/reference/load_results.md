# Load results

This functions loads and checks the results file(s)

## Usage

``` r
load_results(
  df_results_files,
  instrument = "EnVision",
  headers = gDRutils::get_env_identifiers()
)
```

## Arguments

- df_results_files:

  data.table, with datapaths and names of results file(s) or character
  with file path of results file(s)

- instrument:

  character

- headers:

  list of headers identified in the manifest file

## Value

data.table with results' data

## Examples

``` r
 td <- get_test_data()
 r_df <- load_results(result_path(td))
#> INFO [2026-06-02 11:30:02] Reading file /tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data1/RawData_day0.xlsx, sheet Readout_0077vs0068_day7
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#> • `` -> `...5`
#> • `` -> `...6`
#> • `` -> `...7`
#> • `` -> `...8`
#> • `` -> `...9`
#> • `` -> `...10`
#> • `` -> `...11`
#> • `` -> `...12`
#> • `` -> `...13`
#> • `` -> `...14`
#> • `` -> `...15`
#> • `` -> `...16`
#> • `` -> `...17`
#> • `` -> `...18`
#> • `` -> `...19`
#> • `` -> `...20`
#> • `` -> `...21`
#> • `` -> `...22`
#> • `` -> `...23`
#> • `` -> `...24`
#> • `` -> `...25`
#> INFO [2026-06-02 11:30:02] Plate 1 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 2 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 3 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 4 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 5 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 6 read; 384 wells
#> INFO [2026-06-02 11:30:02] File done
#> INFO [2026-06-02 11:30:02] Reading file /tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data1/RawData_day7.xlsx, sheet Readout_0077vs0068_day7
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#> • `` -> `...5`
#> • `` -> `...6`
#> • `` -> `...7`
#> • `` -> `...8`
#> • `` -> `...9`
#> • `` -> `...10`
#> • `` -> `...11`
#> • `` -> `...12`
#> • `` -> `...13`
#> • `` -> `...14`
#> • `` -> `...15`
#> • `` -> `...16`
#> • `` -> `...17`
#> • `` -> `...18`
#> • `` -> `...19`
#> • `` -> `...20`
#> • `` -> `...21`
#> • `` -> `...22`
#> • `` -> `...23`
#> • `` -> `...24`
#> • `` -> `...25`
#> INFO [2026-06-02 11:30:02] Plate 1 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 2 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 3 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 4 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 5 read; 384 wells
#> INFO [2026-06-02 11:30:02] Plate 6 read; 384 wells
#> INFO [2026-06-02 11:30:02] File done
```
