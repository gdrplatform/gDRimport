# Validate gDR input data before import

Checks a set of input files and reports every problem it finds, from
unreadable files to plate-level anomalies in the readouts. Bench input
(a manifest, treatment templates and raw plate-reader files) is loaded
with
[`load_data`](https://gdrplatform.github.io/gDRimport/reference/load_data.md),
so a failure reported for `load_data` is the error the import itself
would raise. The checks also flag problems the loaders let through
without an error: values in a Concentration sheet that are not numbers
(those wells are dropped), raw plates without a barcode, duplicated
wells, plates missing from the manifest or from the data, and Duration
labels that do not fit the readouts of the control wells. PRISM input,
processed MultiAssayExperiments and D300 files are recognised and
checked too.

## Usage

``` r
validate_gdr_input(
  paths,
  type = c("auto", "bench", "prism", "mae", "pre_d300"),
  instrument = NULL,
  id_patterns = list(drug = "^G[0-9]{8}", cellline = NULL)
)

# S3 method for class 'gdr_validation'
print(x, ...)
```

## Arguments

- paths:

  character, paths to input files and/or directories. For a directory,
  the files directly inside it are used, not those in its
  subdirectories.

- type:

  character, the kind of input: `"auto"` (detected from the files),
  `"bench"` (manifest, templates and raw plate-reader files), `"prism"`,
  `"mae"` (a processed gDR MultiAssayExperiment saved as `.qs2`) or
  `"pre_d300"` (D300 `.tdd` files that still need
  [`import_D300`](https://gdrplatform.github.io/gDRimport/reference/import_D300.md)).

- instrument:

  character, the plate-reader format passed to
  [`load_data`](https://gdrplatform.github.io/gDRimport/reference/load_data.md).
  When `NULL`, it is detected from the first raw file with
  [`detect_file_format`](https://gdrplatform.github.io/gDRimport/reference/detect_file_format.md).

- id_patterns:

  list of regular expressions for identifier formats. `drug` is the
  format of a drug identifier once its batch suffix is removed (the
  default is the one
  [`gDRutils::remove_drug_batch`](https://gdrplatform.github.io/gDRstyle/reference/remove_drug_batch.html)
  uses), and `cellline` the format of a cell line identifier (no
  default). An element set to `NULL` switches its check off; elements
  left out keep their default.

- x:

  a `gdr_validation` object.

- ...:

  not used.

## Value

An object of class `gdr_validation`, a list with

- verdict:

  `"PASS"`, `"WARN"` or `"FAIL"`, the most severe finding (`"INFO"`
  counts as `"PASS"`).

- data_type:

  the kind of input that was validated.

- files:

  a `data.table` with one row per input file: `file`, `path`, `role` and
  `format` (the plate-reader format of raw files).

- findings:

  a `data.table` with one row per finding: `check`, `severity`
  (`"PASS"`, `"INFO"`, `"WARN"` or `"FAIL"`), `detail`, `fix_tier`
  (`"A"` a mechanical fix, `"B"` a fix to confirm with whoever made the
  files, `"C"` a decision about the experiment, `NA` when there is
  nothing to fix) and `status_code`, the matching row of
  [`get_exception_data`](https://gdrplatform.github.io/gDRimport/reference/get_exception_data.md)
  (`NA` when there is none).

- fixes:

  character, the repairs applied to the files; always empty for now.

## Details

The input files are only read, never modified.

## Examples

``` r
td <- get_test_data()
res <- validate_gdr_input(c(manifest_path(td), template_path(td), result_path(td)))
res$verdict
#> [1] "PASS"
res$findings
#>                 check severity
#>                <char>   <char>
#> 1:           manifest     PASS
#> 2:   combo_degenerate     INFO
#> 3:          load_data     PASS
#> 4: import_corrections     INFO
#> 5:    duplicate_wells     PASS
#> 6:   barcode_coverage     PASS
#> 7:     plate_outliers     INFO
#>                                                                                                                     detail
#>                                                                                                                     <char>
#> 1:                                                                                          Manifest loaded (12 plate(s)).
#> 2: Template(s) hold the second drug at a single dose, so the combination has no Drug 2 gradient: Template_7daytreated.xlsx
#> 3:                                                                           load_data() read 4608 readout(s) as EnVision.
#> 4:                                                                   load_data() changed the case of 320 untreated tag(s).
#> 5:                                                                                      No barcode and well is read twice.
#> 6:                                                          Every plate in the manifest has data, and the other way round.
#> 7:                                         Not checked: no group of 4 or more plates with the same Duration and cell line.
#>    fix_tier status_code
#>      <char>       <int>
#> 1:     <NA>          NA
#> 2:        C          NA
#> 3:     <NA>          NA
#> 4:     <NA>          NA
#> 5:     <NA>          NA
#> 6:     <NA>          NA
#> 7:     <NA>          NA
```
