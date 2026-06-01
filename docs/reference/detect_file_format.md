# Detect format of results data

Detect format of results data

## Usage

``` r
detect_file_format(results_file)
```

## Arguments

- results_file:

  path to results data

## Value

string of the detected file format

## Examples

``` r
td2 <- get_test_Tecan_data()
detect_file_format(td2$r_files[1])
#> New names:
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#> • `` -> `...6`
#> • `` -> `...7`
#> • `` -> `...8`
#> • `` -> `...9`
#> • `` -> `...10`
#> • `` -> `...11`
#> [1] "Tecan"
```
