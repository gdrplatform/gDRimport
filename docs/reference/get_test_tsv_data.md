# get test tsv data

get test tsv data

## Usage

``` r
get_test_tsv_data()
```

## Value

list with with input data (manifest/template/result paths) and related
reference data (.qs2 file paths)

## Examples

``` r
get_test_tsv_data()
#> $m_file
#> [1] "/tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data5/Manifest.tsv"
#> 
#> $r_files
#> [1] "/tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data5/RawData.tsv"
#> 
#> $t_files
#> [1] "/tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data5/Template_trt.tsv"  
#> [2] "/tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data5/Template_untrt.tsv"
#> 
#> $ref_l_path
#> [1] "/tmp/Rtmpmo65P0/temp_libpath1be42133826f/gDRimport/extdata/data5/ref_l.qs2"
#> 
```
