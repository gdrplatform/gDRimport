# Load long table

Loads a long/tidy `data.table` from a delimited text file (CSV/TSV) and
checks that it contains the columns required by the gDR pipeline. This
offers an input path that does not rely on GeneDataScreeneR: the data
can come from a custom export or be provided directly by the user in
tabular form, matching the shape of the `data_imported` object used
downstream.

## Usage

``` r
load_long_table(long_table_file)
```

## Arguments

- long_table_file:

  character, path to a single CSV/TSV file with the long table. The
  field separator is detected automatically.

## Value

a `data.table` with the validated long table

## Details

Required columns follow the current gDR identifiers: `Gnumber`, `clid`,
`Duration` and `Concentration`. The readout column is the fixed header
`ReadoutValue`: there is no `"readout"` identifier in `gDRutils`, so
this name is not customisable via `get_env_identifiers()`, matching how
`ReadoutValue` is used across the rest of gDRimport. Combination data
may add `Gnumber_2` and `Concentration_2`. `Duration`, `Concentration`
and `ReadoutValue` must be numeric.

## Examples

``` r
 path <- tempfile(fileext = ".csv")
 dt <- data.table::data.table(Gnumber = "G1", clid = "CL1", Duration = 72,
                              Concentration = 1, ReadoutValue = 1000)
 data.table::fwrite(dt, path)
 load_long_table(path)
#>    Gnumber   clid Duration Concentration ReadoutValue
#>     <char> <char>    <num>         <num>        <num>
#> 1:      G1    CL1       72             1         1000
```
