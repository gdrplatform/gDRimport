# Convert a PharmacoSet to a data.table that is prepare for input into gDR pipeline

Convert a PharmacoSet to a data.table that is prepare for input into gDR
pipeline

## Usage

``` r
convert_pset_to_df(pharmacoset, run_parallel = TRUE, workers = 2L)
```

## Arguments

- pharmacoset:

  PharmacoSet object

- run_parallel:

  logical, TRUE (default) if to run functions in Parallel, FALSE to run
  in serial

- workers:

  integer, number of workers defaults to 2L if run_parallel is TRUE

## Value

data.table of PharmacoSet's dose response data with column names aligned
with gDR standard

## Author

Jermiah Joseph – collaboration with BHKLab

## Examples

``` r
pset <- suppressMessages(getPSet(
  "Tavor_2020",
  psetDir = system.file("extdata/pset", package = "gDRimport"),
  use_local_PSets_list = TRUE
))
dt <- convert_pset_to_df(pset)
gDRutils::reset_env_identifiers()
```
