# Load, convert and process the level 6 PRISM data into a gDR input

Load, convert and process the level 6 PRISM data into a gDR input

## Usage

``` r
convert_LEVEL6_prism_to_gDR_input(
  prism_data_path,
  cell_line_data_path,
  treatment_data_path,
  meta_data_path,
  readout_min = 1.03
)
```

## Arguments

- prism_data_path:

  path to PRISM LEVEL6 csv file with collapsed log fold change data

- cell_line_data_path:

  path to cell line info data

- treatment_data_path:

  path to collapsed treatment info data

- meta_data_path:

  path to metadata file describing all cancer models/cell lines which
  are referenced by a dataset contained within the DepMap portal

- readout_min:

  minimum ReadoutValue

## Value

`data.table` object with input data for gDR pipeline

## Examples

``` r
 prism_data_path <- system.file("testdata/prism_collapsed_LOGFC.csv", package = "gDRimport")
 cell_line_data_path <- system.file("testdata/prism_cell_lines.csv", package = "gDRimport")
 treatment_data_path <- system.file("testdata/prism_treatment.csv", package = "gDRimport")
 prism_meta <- system.file("testdata/prism_model.csv", package = "gDRimport")
 convert_LEVEL6_prism_to_gDR_input(prism_data_path, cell_line_data_path, treatment_data_path, prism_meta)
#>         clid      Gnumber       DrugName      drug_moa Duration Concentration
#>       <char>       <char>         <char>        <char>    <num>         <num>
#> 1: some_clid some_drug_id some_drug_name some_drug_moa       NA      2.325889
#> 2: some_clid some_drug_id some_drug_name some_drug_moa       NA      2.325889
#> 3: some_clid      vehicle        vehicle       vehicle       NA      0.000000
#>    ReadoutValue masked CellLineName Tissue parental_identifier subtype
#>           <num> <lgcl>       <char> <char>              <char>  <char>
#> 1:    0.9892612  FALSE    some_clid Breast             unknown unknown
#> 2:    0.9330330  FALSE    some_clid Breast             unknown unknown
#> 3:    1.0000000  FALSE    some_clid Breast             unknown unknown
#>    ReferenceDivisionTime
#>                    <num>
#> 1:                    NA
#> 2:                    NA
#> 3:                    NA
```
