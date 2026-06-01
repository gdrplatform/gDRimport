# check_metadata_names

Check whether all metadata names are correct

## Usage

``` r
check_metadata_names(col_df, df_name = "", df_type = NULL)
```

## Arguments

- col_df:

  a character with colnames of df

- df_name:

  a name of data.table ("" by default)

- df_type:

  a type of a data.table (NULL by default)

## Value

a charvec with corrected colnames of df

## Examples

``` r
 td <- get_test_data()
 m_file <- manifest_path(td)
 m_data <- read_excel_to_dt(m_file)
 result <- check_metadata_names(col_df = colnames(m_data))
```
