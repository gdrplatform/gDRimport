# Extracts an assay from a SummarizedExperiment object or creates a new one if it does not exist

This function takes a SummarizedExperiment object and an assay name as
input. If the specified assay already exists in the SummarizedExperiment
object, it is returned. Otherwise, a new assay with the specified name
is created and added to the SummarizedExperiment object. The new assay
is initialized with NA values. This is useful for when multiple
Summarized Experiments in a given MAE do not have the same assays. And
it is necessary to have the same assays in all Summarized Experiments in
order to convert the MAE to a PSet.

## Usage

``` r
.extract_or_create_assay(SE, assay_name)
```

## Arguments

- SE:

  A SummarizedExperiment object

- assay_name:

  A character string specifying the name of the assay to extract or
  create

## Value

A SummarizedExperiment object with the specified assay
