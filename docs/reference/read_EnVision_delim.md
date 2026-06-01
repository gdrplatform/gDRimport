# Read EnVision delimited text files

This function reads file from the EnVision Workstation

## Usage

``` r
read_EnVision_delim(file, nrows = 10000, seps = c(",", "\t"))
```

## Arguments

- file:

  string to path of input file from EnVision scanner

- nrows:

  maximum number of file rows to be processed

- seps:

  potential field separators of the input file

## Value

a list containing the data table, n_col, n_row, and if is edited
