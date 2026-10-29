# Where gDR input data came from

[`load_data`](https://gdrplatform.github.io/gDRimport/reference/load_data.md)
and the PRISM converters attach a record of their inputs to the table
they return: the gDRimport version, the reader (`instrument`), every
input file with its role, size and md5, and the corrections gDRimport
applied on its own. gDRcore keeps the record in the provenance of the
processed object (`provenance$steps$import`).

## Usage

``` r
get_import_provenance(x)
```

## Arguments

- x:

  the `data` element returned by
  [`load_data`](https://gdrplatform.github.io/gDRimport/reference/load_data.md),
  or a table returned by a PRISM converter.

## Value

The record (a list), or `NULL` when `x` carries none.

## Examples

``` r
td <- get_test_data()
l_data <- load_data(manifest_path(td), template_path(td), result_path(td))
#> INFO [2026-10-29 08:46:44] Manifest loaded successfully
#> INFO [2026-10-29 08:46:44] Reading Template_7daytreated.xlsx with load_templates_xlsx
#> INFO [2026-10-29 08:46:44] Reading Template_Untreated.xlsx with load_templates_xlsx
#> INFO [2026-10-29 08:46:44] Loading Template_7daytreated.xlsx
#> INFO [2026-10-29 08:46:44] Loading Template_Untreated.xlsx
#> INFO [2026-10-29 08:46:44] Templates loaded successfully!
#> INFO [2026-10-29 08:46:44] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day0.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:44] Plate 201904190a read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904190b read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904190c read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904190d read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904190e read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904190f read; 384 wells
#> INFO [2026-10-29 08:46:44] File done
#> INFO [2026-10-29 08:46:44] Reading file /tmp/RtmpeJJadn/temp_libpath1af3380ffba5/gDRimport/extdata/data1/RawData_day7.xlsx, sheet Readout_0077vs0068_day7
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
#> INFO [2026-10-29 08:46:44] Plate 201904197a read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904197b read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904197c read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904197d read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904197e read; 384 wells
#> INFO [2026-10-29 08:46:44] Plate 201904197f read; 384 wells
#> INFO [2026-10-29 08:46:44] File done
get_import_provenance(l_data$data)$files
#>       role                      name  size                              md5
#> 1 manifest             manifest.xlsx  9072 9cde387345bf776b4dfa1ea1d50d96d4
#> 2 template Template_7daytreated.xlsx 26369 7e798374f8c622b0eaaae340ef29896c
#> 3 template   Template_Untreated.xlsx  9419 11fe28b8db25cadb6e0cdb6e9e88ed4c
#> 4  results         RawData_day0.xlsx 26897 e66d663e9d3d11a8f45d608b9692956e
#> 5  results         RawData_day7.xlsx 33128 9de5953a22dc2142383466113dc3743a
```
