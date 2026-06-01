# Load templates

This functions loads and checks the template file(s)

## Usage

``` r
load_templates(df_template_files)
```

## Arguments

- df_template_files:

  data.table, with datapaths and names of results file(s) or character
  with file path of templates file(s)

## Value

data.table with templates data

## Examples

``` r
 td <- get_test_data()
 t_df <- load_templates(template_path(td))
#> INFO [2026-06-01 16:44:34] Reading Template_7daytreated.xlsx with load_templates_xlsx
#> INFO [2026-06-01 16:44:34] Reading Template_Untreated.xlsx with load_templates_xlsx
#> INFO [2026-06-01 16:44:34] Loading Template_7daytreated.xlsx
#> INFO [2026-06-01 16:44:34] Loading Template_Untreated.xlsx
#> INFO [2026-06-01 16:44:34] Templates loaded successfully!
```
