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

  character; `.tsv` files with `"EnVision"` are read as `"long_tsv"`,
  whether it is passed or left as the default

- headers:

  list of headers identified in the manifest file

## Value

data.table with results' data

## Examples

``` r
 td <- get_test_data()
 r_df <- load_results(result_path(td))
#> INFO [2026-10-29 08:46:49] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day0.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:50] Plate 201904190a read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904190b read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904190c read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904190d read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904190e read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904190f read; 384 wells
#> INFO [2026-10-29 08:46:50] File done
#> INFO [2026-10-29 08:46:50] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day7.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:50] Plate 201904197a read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904197b read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904197c read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904197d read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904197e read; 384 wells
#> INFO [2026-10-29 08:46:50] Plate 201904197f read; 384 wells
#> INFO [2026-10-29 08:46:50] File done
```
