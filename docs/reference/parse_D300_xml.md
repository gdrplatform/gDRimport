# Parse D300

This function parses a D300 \*.tdd file (XML format) into a data.table

## Usage

``` r
parse_D300_xml(D300_file)
```

## Arguments

- D300_file:

  string, file path to D300 .tdd file

## Value

data.table representing input `D300_file`.

## Examples

``` r
td3 <- get_test_D300_data()
fs <- td3[["f_96w"]]
dose_df <- parse_D300_xml(fs[["d300"]])
```
