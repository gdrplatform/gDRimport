# get test Tecan data

get test Tecan data

## Usage

``` r
get_test_Tecan_data()
```

## Value

list with with input data (manifest/template/result paths) and related
reference data (qs2 file paths)

## Examples

``` r
get_test_Tecan_data()
#> $m_file
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/manifest_Tecan_96_well_plates.xlsx"
#> 
#> $r_files
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/RawData_Tecan_96_well_plates.xlsx"
#> 
#> $t_files
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/D300_trt_Tecan_96_well_plates.xlsx"
#> 
#> $ref_m_df
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/ref_manifest_Tecan_96_well_plates.qs2"
#> 
#> $ref_r_df
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/ref_RawData_Tecan_96_well_plates.qs2"
#> 
#> $ref_t_df
#> [1] "/tmp/RtmppUXNua/temp_libpath282139094257/gDRimport/extdata/data2/ref_D300_trt_Tecan_96_well_plates.qs2"
#> 
```
