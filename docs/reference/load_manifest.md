# Load manifest

This functions loads and checks the manifest file(s)

## Usage

``` r
load_manifest(manifest_file)
```

## Arguments

- manifest_file:

  character, file path(s) to manifest(s)

## Value

list with manifest data.table and headers

## Examples

``` r
 td <- get_test_data()
 ml <- load_manifest(manifest_path(td))
#> INFO [2026-06-02 11:30:02] Manifest loaded successfully
```
