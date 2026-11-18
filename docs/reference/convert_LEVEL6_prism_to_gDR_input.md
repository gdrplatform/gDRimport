# Load, convert and process the level 6 PRISM data into a gDR input

Reads collapsed log fold changes from the PRISM Repurposing screens (LFC
matrix or long `row_id`/`profile_id` table) and from the DepMap PRISM
Oncology Reference (LFC matrix up to 24Q4, long `LFCCollapsed` tables of
the Luminex and sequencing readouts from 25Q2).

## Usage

``` r
convert_LEVEL6_prism_to_gDR_input(
  prism_data_path,
  cell_line_data_path = NULL,
  treatment_data_path,
  meta_data_path,
  readout_min = 1.03,
  dose_units = "uM"
)
```

## Arguments

- prism_data_path:

  path to PRISM LEVEL6 csv file with collapsed log fold change data

- cell_line_data_path:

  path to cell line info data, or `NULL`. Cell lines are taken from the
  LFC data, so the file is only recorded in the import provenance; the
  sequencing readout of DepMap OncRef comes without one.

- treatment_data_path:

  path to collapsed treatment info data

- meta_data_path:

  path to metadata file describing all cancer models/cell lines which
  are referenced by a dataset contained within the DepMap portal

- readout_min:

  minimum ReadoutValue

- dose_units:

  dose units to keep when the data has a `pert_dose_unit` column (DepMap
  OncRef from 25Q2 has antibodies and ADCs in ug/mL), or `NULL` to keep
  all. gDR expects concentrations in uM.

## Value

`data.table` object with input data for gDR pipeline

## Details

Cell lines are matched to `ModelID` in the model metadata by their
DepMap ID. Cell lines that failed STR profiling (`_FAILED_STR` in the
ID) and cell lines missing from the model metadata are dropped. The cell
line ID is the `CCLEName`, or the `ModelID` for models without one.
Measurements that DepMap flags as outliers are kept with
`masked = TRUE`. Everything dropped or masked is listed in
`get_import_provenance(x)$corrections`.

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
