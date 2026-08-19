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
#> [1] "/tmp/RtmpUkSU5c/temp_libpath1ae074ba296b/gDRimport/extdata/data5/Manifest.tsv"
#> 
#> $r_files
#> [1] "/tmp/RtmpUkSU5c/temp_libpath1ae074ba296b/gDRimport/extdata/data5/RawData.tsv"
#> 
#> $t_files
#> [1] "/tmp/RtmpUkSU5c/temp_libpath1ae074ba296b/gDRimport/extdata/data5/Template_trt.tsv"  
#> [2] "/tmp/RtmpUkSU5c/temp_libpath1ae074ba296b/gDRimport/extdata/data5/Template_untrt.tsv"
#> 
#> $ref_l_path
#> [1] "/tmp/RtmpUkSU5c/temp_libpath1ae074ba296b/gDRimport/extdata/data5/ref_l.qs2"
#> 
```
