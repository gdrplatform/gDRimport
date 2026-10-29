# Load data

This functions loads and checks the data file(s)

## Usage

``` r
load_data(
  manifest_file,
  df_template_files,
  results_file,
  instrument = "EnVision"
)
```

## Arguments

- manifest_file:

  character, file path(s) to manifest(s)

- df_template_files:

  data.table, with datapaths and names of results file(s) or character
  with file path of templates file(s)

- results_file:

  data.table, with datapaths and names of results file(s) or character
  with file path of results file(s)

- instrument:

  character

## Value

a list with three data.tables for manifest/treatment and results

## Examples

``` r
 td <- get_test_data()
 l_tbl <- load_data(manifest_path(td), template_path(td), result_path(td))
#> INFO [2026-10-29 08:46:47] Manifest loaded successfully
#> INFO [2026-10-29 08:46:47] Reading Template_7daytreated.xlsx with load_templates_xlsx
#> INFO [2026-10-29 08:46:47] Reading Template_Untreated.xlsx with load_templates_xlsx
#> INFO [2026-10-29 08:46:47] Loading Template_7daytreated.xlsx
#> INFO [2026-10-29 08:46:47] Loading Template_Untreated.xlsx
#> INFO [2026-10-29 08:46:47] Templates loaded successfully!
#> INFO [2026-10-29 08:46:47] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day0.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:47] Plate 201904190a read; 384 wells
#> INFO [2026-10-29 08:46:47] Plate 201904190b read; 384 wells
#> INFO [2026-10-29 08:46:47] Plate 201904190c read; 384 wells
#> INFO [2026-10-29 08:46:47] Plate 201904190d read; 384 wells
#> INFO [2026-10-29 08:46:47] Plate 201904190e read; 384 wells
#> INFO [2026-10-29 08:46:47] Plate 201904190f read; 384 wells
#> INFO [2026-10-29 08:46:47] File done
#> INFO [2026-10-29 08:46:47] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day7.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:48] Plate 201904197a read; 384 wells
#> INFO [2026-10-29 08:46:48] Plate 201904197b read; 384 wells
#> INFO [2026-10-29 08:46:48] Plate 201904197c read; 384 wells
#> INFO [2026-10-29 08:46:48] Plate 201904197d read; 384 wells
#> INFO [2026-10-29 08:46:48] Plate 201904197e read; 384 wells
#> INFO [2026-10-29 08:46:48] Plate 201904197f read; 384 wells
#> INFO [2026-10-29 08:46:48] File done
```
