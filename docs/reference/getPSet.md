# Get PharmacoSet

Get PharmacoSet

## Usage

``` r
getPSet(
  pset_name,
  psetDir = getwd(),
  canonical = FALSE,
  timeout = 600,
  use_local_PSets_list = FALSE
)
```

## Arguments

- pset_name:

  string with the name of the PharmacoSet

- psetDir:

  string with the temporary directory for the PharmacoSet

- canonical:

  logical flag indicating if the PSet canonical

- timeout:

  maximum number of seconds allowed for PSet download

- use_local_PSets_list:

  logical flag if PSets list should be used from local. If FALSE PSets
  list will be taken from web.

## Value

PharmacoSet object

## Examples

``` r
suppressMessages(getPSet(
  "Tavor_2020",
  psetDir = system.file("extdata/pset", package = "gDRimport"),
  use_local_PSets_list = TRUE
))
#> <PharmacoSet>
#> Name: Tavor 
#> Date Created: Fri Feb  4 17:32:17 2022 
#> Number of samples:  53 
#> Molecular profiles: 
#> RNAseq :
#>   Dim: 24137, 43 
#> Treatment response: Drug pertubation:
#>    Please look at pertNumber(cSet) to determine number of experiments  for each drug-sample combination.
#> Drug sensitivity:
#>    Number of Experiments:  2668 
#>    Please look at sensNumber(cSet) to determine number of  experiments for each drug-sample combination.
```
