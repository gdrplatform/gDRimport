# standardize_record_values

map values to a dictionary

## Usage

``` r
standardize_record_values(x, dictionary = DICTIONARY)
```

## Arguments

- x:

  a named array

- dictionary:

  a named array

## Value

a named array with updated names

## Examples

``` r
standardize_record_values(c("Vehicle", "vehcle"))
#> [1] "vehicle" "vehicle"
```
