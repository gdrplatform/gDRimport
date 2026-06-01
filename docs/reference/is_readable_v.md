# is_readable_v Check if all paths in vector are readable

is_readable_v Check if all paths in vector are readable

## Usage

``` r
is_readable_v(paths)
```

## Arguments

- paths:

  a character with path(s)

## Value

`NULL` invisibly.

## Examples

``` r
td2 <- get_test_Tecan_data()
is_readable_v(td2$r_files)
#> [1] TRUE
```
