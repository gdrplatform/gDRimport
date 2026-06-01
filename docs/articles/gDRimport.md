# gDRimport

## Overview

The `gDRimport` package is a part of the gDR suite. It helps to prepare
raw drug response data for downstream processing. It mainly contains
helper functions for importing/loading/validating dose response data
provided in different file formats.

## Use Cases

### Test Data

There are currently four test datasets that can be used to see what’s
the expected input data for the gDRimport.

``` r
# primary test data
td1 <- get_test_data()
summary(td1)
```

    ##        Length         Class          Mode 
    ##             1 gdr_test_data            S4

``` r
td1
```

    ## class: gdr_test_data 
    ## slots: manifest_path result_path template_path ref_m_df ref_r1_r2 ref_r1 ref_t1_t2 ref_t1

``` r
# test data in Tecan format
td2 <- get_test_Tecan_data()
summary(td2)
```

    ##          Length Class  Mode     
    ## m_file   1      -none- character
    ## r_files  1      -none- character
    ## t_files  1      -none- character
    ## ref_m_df 1      -none- character
    ## ref_r_df 1      -none- character
    ## ref_t_df 1      -none- character

``` r
# test data in D300 format
td3 <- get_test_D300_data()
summary(td3)
```

    ##        Length Class  Mode
    ## f_96w  6      -none- list
    ## f_384w 6      -none- list

``` r
# test data obtained from EnVision
td4 <- get_test_EnVision_data()
summary(td4)
```

    ##            Length Class  Mode     
    ## m_file      1     -none- character
    ## r_files    28     -none- character
    ## t_files     2     -none- character
    ## ref_l_path  1     -none- character

### Load data

The `load_data` is the key function. It wraps `load_manifest`,
`load_templates` and `load_results` functions and supports different
file formats.

``` r
ml <- load_manifest(manifest_path(td1))
summary(ml)
```

    ##         Length Class      Mode
    ## data     4     data.table list
    ## headers 27     -none-     list

``` r
t_df <- load_templates(template_path(td1))
summary(t_df)
```

    ##       WellRow        WellColumn       Gnumber      Concentration
    ##  Length   :768   Length   :768   Length   :768   Length   :768  
    ##  N.unique : 16   N.unique : 24   N.unique :  3   N.unique : 10  
    ##  N.blank  :  0   N.blank  :  0   N.blank  :  0   N.blank  :  0  
    ##  Min.nchar:  1   Min.nchar:  1   Min.nchar:  6   Min.nchar:  1  
    ##  Max.nchar:  1   Max.nchar:  2   Max.nchar:  7   Max.nchar: 19  
    ##                                  NAs      :128   NAs      :448  
    ##      Gnumber_2    Concentration_2      Template  
    ##  Length   :768   Length   :768    Length   :768  
    ##  N.unique :  2   N.unique :  2    N.unique :  2  
    ##  N.blank  :  0   N.blank  :  0    N.blank  :  0  
    ##  Min.nchar:  6   Min.nchar:  1    Min.nchar: 23  
    ##  Max.nchar:  7   Max.nchar:  4    Max.nchar: 25  
    ##  NAs      :448   NAs      :448

``` r
r_df <- suppressMessages(load_results(result_path(td1)))
summary(r_df)
```

    ##       Barcode          WellRow       WellColumn     ReadoutValue    
    ##  Length   :4587   Length   :4587   Min.   : 1.00   Min.   :  12627  
    ##  N.unique :   6   N.unique :  16   1st Qu.: 6.50   1st Qu.:  67905  
    ##  N.blank  :   0   N.blank  :   0   Median :12.00   Median : 140865  
    ##  Min.nchar:   1   Min.nchar:   1   Mean   :12.49   Mean   : 263996  
    ##  Max.nchar:   1   Max.nchar:   1   3rd Qu.:18.00   3rd Qu.: 324707  
    ##                                    Max.   :24.00   Max.   :2423054  
    ##  BackgroundValue
    ##  Min.   :332.0  
    ##  1st Qu.:351.0  
    ##  Median :374.0  
    ##  Mean   :453.2  
    ##  3rd Qu.:570.0  
    ##  Max.   :704.0

``` r
l_tbl <-
  suppressMessages(
    load_data(manifest_path(td1), template_path(td1), result_path(td1)))
summary(l_tbl)
```

    ##            Length Class      Mode
    ## manifest   4      data.table list
    ## treatments 7      data.table list
    ## data       5      data.table list

## PRISM

PRISM, the Multiplexed cancer cell line screening platform, facilitates
rapid screening of a broad spectrum of drugs across more than 900 human
cancer cell line models, employing a high-throughput, multiplexed
approach. Publicly available PRISM data can be downloaded from the
DepMap website ([DepMap](https://depmap.org/portal/download/all/)).

The `gDRimport` package provides support for processing PRISM data at
two levels: LEVEL5 and LEVEL6.

- LEVEL5 Data: This format encapsulates all information about drugs,
  cell lines, and viability within a single file. To process LEVEL5
  PRISM data, you can use the
  [`convert_LEVEL5_prism_to_gDR_input()`](https://gdrplatform.github.io/gDRimport/reference/convert_LEVEL5_prism_to_gDR_input.md)
  function. This function not only transforms and cleans the data but
  also executes the gDR pipeline for further analysis.

- LEVEL6 Data: In LEVEL6, PRISM data is distributed across three
  separate files:

prism_data: containing collapsed log fold change data for viability
assays. cell_line_data: providing information about cell lines.
treatment_data: containing treatment data.

Processing LEVEL6 PRISM data can be accomplished using the
[`convert_LEVEL6_prism_to_gDR_input()`](https://gdrplatform.github.io/gDRimport/reference/convert_LEVEL6_prism_to_gDR_input.md)
function, which requires paths to these three files as input arguments.

#### Processing LEVEL5 PRISM Data

To process LEVEL5 PRISM data, you can use the following function:

``` r
convert_LEVEL5_prism_to_gDR_input("path_to_file")
```

Replace “path_to_file” with the actual path to your LEVEL5 PRISM data
file. This function will handle the transformation, cleaning, and
execution of the gDR pipeline automatically.

#### Processing LEVEL6 PRISM Data

To process LEVEL6 PRISM data, you can use the following function:

``` r
convert_LEVEL6_prism_to_gDR_input("prism_data_path", "cell_line_data_path", "treatment_data_path")
```

Replace “prism_data_path”, “cell_line_data_path”, and
“treatment_data_path” with the respective paths to your LEVEL6 PRISM
data files.

### Package installation

The function `installAllDeps` assists in installing package
dependencies.

## SessionInfo

``` r
sessionInfo()
```

    ## R version 4.6.0 (2026-04-24)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] gDRimport_1.11.3 BiocStyle_2.40.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] sass_0.4.10                 generics_0.1.4             
    ##  [3] SparseArray_1.12.2          futile.options_1.0.1       
    ##  [5] stringi_1.8.7               lattice_0.22-9             
    ##  [7] rematch_2.0.0               digest_0.6.39              
    ##  [9] magrittr_2.0.5              grid_4.6.0                 
    ## [11] evaluate_1.0.5              bookdown_0.46              
    ## [13] fastmap_1.2.0               Matrix_1.7-5               
    ## [15] cellranger_1.1.0            jsonlite_2.0.0             
    ## [17] backports_1.5.1             formatR_1.14               
    ## [19] BiocManager_1.30.27         textshaping_1.0.5          
    ## [21] jquerylib_0.1.4             abind_1.4-8                
    ## [23] cli_3.6.6                   rlang_1.2.0                
    ## [25] XVector_0.52.0              futile.logger_1.4.9        
    ## [27] Biobase_2.72.0              DelayedArray_0.38.2        
    ## [29] cachem_1.1.0                yaml_2.3.12                
    ## [31] otel_0.2.0                  S4Arrays_1.12.0            
    ## [33] tools_4.6.0                 checkmate_2.3.4            
    ## [35] SummarizedExperiment_1.42.0 lambda.r_1.2.4             
    ## [37] gDRutils_1.10.0             BiocGenerics_0.58.1        
    ## [39] assertthat_0.2.1            vctrs_0.7.3                
    ## [41] R6_2.6.1                    matrixStats_1.5.0          
    ## [43] stats4_4.6.0                lifecycle_1.0.5            
    ## [45] Seqinfo_1.2.0               S4Vectors_0.50.1           
    ## [47] fs_2.1.0                    htmlwidgets_1.6.4          
    ## [49] IRanges_2.46.0              ragg_1.5.2                 
    ## [51] pkgconfig_2.0.3             desc_1.4.3                 
    ## [53] pkgdown_2.2.0               pillar_1.11.1              
    ## [55] bslib_0.11.0                data.table_1.18.4          
    ## [57] glue_1.8.1                  systemfonts_1.3.2          
    ## [59] GenomicRanges_1.64.0        xfun_0.57                  
    ## [61] tibble_3.3.1                MatrixGenerics_1.24.0      
    ## [63] knitr_1.51                  htmltools_0.5.9            
    ## [65] rmarkdown_2.31              compiler_4.6.0             
    ## [67] readxl_1.5.0
