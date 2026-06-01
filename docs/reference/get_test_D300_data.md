# get test D300 data

get test D300 data

## Usage

``` r
get_test_D300_data()
```

## Value

list with with input data (manifest/template/result paths) and related
reference data (qs2 file paths)

## Examples

``` r
get_test_D300_data()
#> $f_96w
#> $f_96w$d300
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/D300_96_well_plate_example.tdd"
#> 
#> $f_96w$Gnum
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/Gnumber_D300_96_well_plate.xlsx"
#> 
#> $f_96w$dest_path
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/output_files_96w/output"
#> 
#> $f_96w$ref_d300
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/ref_D300_96_well_plate_example.qs2"
#> 
#> $f_96w$ref_Gnum
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/ref_Gnumber_D300_96_well_plate.qs2"
#> 
#> $f_96w$ref_output_path
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/output_files_96w/reference"
#> 
#> 
#> $f_384w
#> $f_384w$d300
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/D300_384_well_plate_example.tdd"
#> 
#> $f_384w$Gnum
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/Gnumber_D300_384_well_plate.xlsx"
#> 
#> $f_384w$ref_Gnum
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/ref_Gnumber_D300_384_well_plate.qs2"
#> 
#> $f_384w$ref_d300
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/ref_D300_384_well_plate_example.qs2"
#> 
#> $f_384w$dest_path
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/output_files_384w/output"
#> 
#> $f_384w$ref_output_path
#> [1] "/tmp/RtmpW1W92u/temp_libpath287971bdee5e/gDRimport/extdata/data3/output_files_384w/reference"
#> 
#> 
```
