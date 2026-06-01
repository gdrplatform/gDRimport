# Load, convert and process the level 5 PRISM data into a gDR input

Load, convert and process the level 5 PRISM data into a gDR input

## Usage

``` r
convert_LEVEL5_prism_to_gDR_input(
  prism_data_path,
  meta_data_path,
  readout_min = 1.03
)
```

## Arguments

- prism_data_path:

  path to PRISM LEVEL5 csv file with data

- meta_data_path:

  path to metadata file describing all cancer models/cell lines which
  are referenced by a dataset contained within the DepMap portal

- readout_min:

  minimum ReadoutValue

## Value

`data.table` object with input data for gDR pipeline

## Examples

``` r
 prism_data <- system.file("testdata/prism_sa.csv", package = "gDRimport")
 prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")
 convert_LEVEL5_prism_to_gDR_input(prism_data, prism_meta)
#> Warning: NAs introduced by coercion
#>         clid CellLineName Tissue parental_identifier subtype
#>       <char>       <char> <char>              <char>  <char>
#> 1: some_clid    some_clid Breast             unknown unknown
#> 2: some_clid    some_clid Breast             unknown unknown
#> 3: some_clid    some_clid Breast             unknown unknown
#> 4: some_clid    some_clid Breast             unknown unknown
#>    ReferenceDivisionTime Duration ReadoutValue BackgroundValue     Gnumber
#>                    <num>    <num>        <num>           <num>      <char>
#> 1:                    NA      120         1.03               0 someGnumber
#> 2:                    NA      240         1.03               0 someGnumber
#> 3:                    NA      120         1.00               0     vehicle
#> 4:                    NA      240         1.00               0     vehicle
#>    Concentration masked
#>            <num> <lgcl>
#> 1:      0.003201  FALSE
#> 2:      0.003201  FALSE
#> 3:      0.000000  FALSE
#> 4:      0.000000  FALSE
```
