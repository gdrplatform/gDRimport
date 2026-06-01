# Import D300

This functions takes a D300 file and generates corresponding template
files

## Usage

``` r
import_D300(D300_file, destination_path, metadata_file = NULL, day0 = FALSE)
```

## Arguments

- D300_file:

  character, file path to D300 file

- destination_path:

  character, path to folder where template files will be generated

- metadata_file:

  character, file path to file with mapping from D300 names to Gnumbers.
  Defaults to NULL.

- day0:

  logical, if TRUE, creates a template file for Day 0 data filled with
  vehicles in addition to the standard plates. Defaults to FALSE.

## Value

Create one Excel file per plate. Each sheet in each plate file describes
the drugs and corrresponding concentrations of what was tested in each
well.

## Details

For example, wells treated with 2 drugs in combination will result in 4
sheets per plate.

- Sheet 1: Drug 1

- Sheet 2: Conc of Drug 1

- Sheet 3: Drug 2

- Sheet 4: Conc of Drug 2

## Examples

``` r
td3 <- get_test_D300_data()[["f_96w"]]
o_path <- file.path(tempdir(), "td3")
dir.create(o_path)
import_D300(td3$d300, o_path, td3$Gnum)
list.files(o_path)
#>  [1] "trt_P1.xlsx"  "trt_P10.xlsx" "trt_P11.xlsx" "trt_P12.xlsx" "trt_P13.xlsx"
#>  [6] "trt_P14.xlsx" "trt_P15.xlsx" "trt_P16.xlsx" "trt_P17.xlsx" "trt_P18.xlsx"
#> [11] "trt_P19.xlsx" "trt_P2.xlsx"  "trt_P20.xlsx" "trt_P21.xlsx" "trt_P22.xlsx"
#> [16] "trt_P3.xlsx"  "trt_P4.xlsx"  "trt_P5.xlsx"  "trt_P6.xlsx"  "trt_P7.xlsx" 
#> [21] "trt_P8.xlsx"  "trt_P9.xlsx" 
unlink(o_path, recursive = TRUE)
```
