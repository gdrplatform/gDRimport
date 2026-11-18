# Read the curve fits delivered by PRISM

Reads a PRISM dose-response table (the `DRC_TABLE` of an MTS or EPS
delivery), with one row per curve PRISM fitted. The cell line identifier
and Duration are added in the form
[`convert_LEVEL5_prism_to_gDR_input`](https://gdrplatform.github.io/gDRimport/reference/convert_LEVEL5_prism_to_gDR_input.md)
gives them, so the curves can be matched to the processed data, e.g. by
`gDRcore::add_PRISM_metrics()`, which adds them to the `Metrics` assay
as a second fit source.

## Usage

``` r
read_PRISM_drc(drc_path, meta_data_path = NULL)
```

## Arguments

- drc_path:

  string, path to the PRISM DRC table (CSV).

- meta_data_path:

  string, path to the DepMap `Model.csv` that maps `depmap_id` to the
  CCLE name (the file given to
  [`convert_LEVEL5_prism_to_gDR_input()`](https://gdrplatform.github.io/gDRimport/reference/convert_LEVEL5_prism_to_gDR_input.md)).
  Not needed when the table has a `ccle_name` column.

## Value

`data.table` with the columns of the DRC table, the cell line identifier
(the CCLE name, under the name of the `cellline` identifier) and
`Duration` in hours (`day * 24`). Rows whose `depmap_id` has no CCLE
name in `Model.csv` get `NA`, as they are left out of the LEVEL5 import.
The input files are recorded in the attribute read by
[`get_import_provenance`](https://gdrplatform.github.io/gDRimport/reference/get_import_provenance.md).

## Details

PRISM fits
`upper_limit + (lower_limit - upper_limit) / (1 + (dose / inflection)^slope)`;
the table is returned as delivered, without any change to these values.

## Examples

``` r
curves <- read_PRISM_drc(
  drc_path = system.file("testdata/prism_drc.csv", package = "gDRimport"),
  meta_data_path = system.file("testdata/prism_drc_model.csv", package = "gDRimport")
)
curves[, c("clid", "Duration", "pert_name", "lower_limit", "upper_limit", "slope", "inflection")]
#>                                           clid Duration pert_name lower_limit
#>                                         <char>    <num>    <char>       <num>
#> 1:      HEL_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE      120    G00106        0.21
#> 2:  HEL9217_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE      120    G00106        0.35
#> 3:  HEL9217_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE      120    G00106        0.29
#> 4: MONOMAC6_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE      120    G00106          NA
#>    upper_limit slope inflection
#>          <num> <num>      <num>
#> 1:        0.98  -1.4       0.85
#> 2:        1.01  -0.9       2.40
#> 3:        0.96  -1.1       1.70
#> 4:          NA    NA         NA
```
