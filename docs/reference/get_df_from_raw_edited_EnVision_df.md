# Get final results (as a data.table) from raw edited EnVision data.table

Get final results (as a data.table) from raw edited EnVision data.table

## Usage

``` r
get_df_from_raw_edited_EnVision_df(
  df,
  barcode_idx,
  barcode_col,
  n_row,
  n_col,
  fname,
  sheet_name,
  headers
)
```

## Arguments

- df:

  raw data.table

- barcode_idx:

  numeric vector with barcode indices

- barcode_col:

  column number for barcode data

- n_row:

  number of rows

- n_col:

  number of columns

- fname:

  file name

- sheet_name:

  name of the Excel sheet

- headers:

  list with the headersa

## Value

data.table derived from EnVision data
