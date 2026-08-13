# Converting PharmacoSet Drug Response Data into gDR object

``` r
library(PharmacoGx)
#> Loading required package: CoreGx
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: 'generics'
#> The following objects are masked from 'package:base':
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: 'BiocGenerics'
#> The following objects are masked from 'package:stats':
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from 'package:base':
#> 
#>     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
#>     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
#>     get, grep, grepl, is.unsorted, lapply, Map, mapply, match, mget,
#>     order, paste, pmax, pmax.int, pmin, pmin.int, Position, rank,
#>     rbind, Reduce, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: SummarizedExperiment
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: 'MatrixGenerics'
#> The following objects are masked from 'package:matrixStats':
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: GenomicRanges
#> Loading required package: stats4
#> Loading required package: S4Vectors
#> 
#> Attaching package: 'S4Vectors'
#> The following object is masked from 'package:utils':
#> 
#>     findMatches
#> The following objects are masked from 'package:base':
#> 
#>     expand.grid, I, unname
#> Loading required package: IRanges
#> Loading required package: Seqinfo
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: 'Biobase'
#> The following object is masked from 'package:MatrixGenerics':
#> 
#>     rowMedians
#> The following objects are masked from 'package:matrixStats':
#> 
#>     anyMissing, rowMedians
#> 
#> Attaching package: 'PharmacoGx'
#> The following objects are masked from 'package:CoreGx':
#> 
#>     .parseToRoxygen, amcc, connectivityScore, cosinePerm, gwc, mcc
library(gDRimport)
```

## Overview

The `gDRimport` package is a part of the gDR suite. It helps to prepare
raw drug response data for downstream processing. It mainly contains
helper functions for importing/loading/validating dose response data
provided from different scanner sources. In collaboration with the
BHKLab, `gDRimport` also provides functions that can convert a
`PharmacoGx::PharamcoSet` object into a gDR object. With this
functionality, users familiar with the gDR suite of packages and methods
can utilize the publically available, curated datasets from the
PharmacoGx database. The main step in this process is to extract the
drug dose-response data from the PharmacoSets and transform them into a
`data.table` that can be used as input for the
`gDRcore::runDrugResponseProcessingPipeline`.

## Loading a PharmacoSet (PSet)

Whereas a user might already have a pharmacoset loaded in their R
session, if they wish to obtain a different pharmacoset or use the same
script in the future we provide a helper function to do so. It helps to
have a user directory in which to store all pharmacosets, and by passing
this directory into the function as a parameter, the function will also
check to see if the PSet exists in the user-defined directory. This is
to ensure that the PSet is not being re-downloaded if it already has.

``` r
pset <- getPSet("Tavor_2020")
pset
```

## Converting PharmacoSet to data.table for gDR pipeline

`PharamcoSets` hold data pertaining to the cell lines (@sample slot),
drugs (@treatment slot), and dose response experiments
(@treatmentResponse slot). The dose response data is stored in a
`treatmentResponseExperiment` object and the function
[`gDRimport::convert_pset_to_df`](https://gdrplatform.github.io/gDRimport/reference/convert_pset_to_df.md)
extracts this information to build a `data.table` that can be used as
input to the gDR pipeline.

``` r
# Store treatment response data in df_
dt <- convert_pset_to_df(pharmacoset = pset)
str(dt)
#> Classes 'data.table' and 'data.frame':   34516 obs. of  7 variables:
#>  $ Barcode              : chr  "PCM-0103090_1_130695_2018-08-28" "PCM-0103090_1_130695_2018-08-28" "PCM-0103090_1_130695_2018-08-28" "PCM-0103090_1_130695_2018-08-28" ...
#>  $ ReadoutValue         : num  75.7 63.1 75.9 87.2 78.8 ...
#>  $ Concentration        : num  0.0021 0.0052 0.01 0.0299 0.0798 ...
#>  $ Clid                 : chr  "130695" "130695" "130695" "130695" ...
#>  $ DrugName             : chr  "Ivosidenib" "Ivosidenib" "Ivosidenib" "Ivosidenib" ...
#>  $ Duration             : num  48 48 48 48 48 48 48 48 48 48 ...
#>  $ ReferenceDivisionTime: logi  NA NA NA NA NA NA ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x55a1866fa050>
```

## Subsetting to extract relevant information

Most canonical `PharmacoSets` have data pertaining to many cell lines
and their response to many drugs (drug-combination data is available in
some but its conversion to gDR is not currently supported). As such, in
the interest of time and resources, it may be useful to subset the data
before providing it as input for the gDR pipeline.

``` r
# example subset using only 1 cell line
subset_cl <- dt$Clid[1]
x <- dt[Clid == subset_cl]
x
#>                              Barcode ReadoutValue Concentration   Clid
#>                               <char>        <num>         <num> <char>
#>   1: PCM-0103090_1_130695_2018-08-28       75.733        0.0021 130695
#>   2: PCM-0103090_1_130695_2018-08-28       63.094        0.0052 130695
#>   3: PCM-0103090_1_130695_2018-08-28       75.935        0.0100 130695
#>   4: PCM-0103090_1_130695_2018-08-28       87.159        0.0299 130695
#>   5: PCM-0103090_1_130695_2018-08-28       78.766        0.0798 130695
#>  ---                                                                  
#> 589: PCM-0064526_5_130695_2018-08-28       71.707        1.4960 130695
#> 590: PCM-0064526_5_130695_2018-08-28       60.488        3.9900 130695
#> 591: PCM-0064526_5_130695_2018-08-28       25.366        9.9750 130695
#> 592: PCM-0064526_5_130695_2018-08-28        5.976       24.9400 130695
#> 593: PCM-0064526_5_130695_2018-08-28      100.000        0.0000 130695
#>         DrugName Duration ReferenceDivisionTime
#>           <char>    <num>                <lgcl>
#>   1:  Ivosidenib       48                    NA
#>   2:  Ivosidenib       48                    NA
#>   3:  Ivosidenib       48                    NA
#>   4:  Ivosidenib       48                    NA
#>   5:  Ivosidenib       48                    NA
#>  ---                                           
#> 589: Azacitidine       48                    NA
#> 590: Azacitidine       48                    NA
#> 591: Azacitidine       48                    NA
#> 592: Azacitidine       48                    NA
#> 593:     vehicle       48                    NA
```

## Running drug response pipeline with data

The subsetted data can now be used as input for the
`gDRcore::runDrugResponseProcessingPipeline()`. The output of this
function is a `MultiAssayExperiment` object which can be accessed with
[`gDRutils::convert_se_assay_to_dt()`](https://gdrplatform.github.io/gDRstyle/reference/convert_se_assay_to_dt.html)

``` r
# RUN DRUG RESPONSE PROCESSING PIPELINE
se <- gDRcore::runDrugResponseProcessingPipeline(x)
se
```

``` r
# Convert Summarized Experiments to data.table
# Available SEs : "RawTreatred", "Controls", "Normalized", "Averaged", "Metrics"

str(gDRutils::convert_se_assay_to_dt(se[[1]], "Averaged"))
str(gDRutils::convert_se_assay_to_dt(se[[1]], "Metrics"))
```

## SessionInfo

``` r
sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] gDRimport_1.11.5            PharmacoGx_3.16.0          
#>  [3] CoreGx_2.16.0               SummarizedExperiment_1.42.0
#>  [5] Biobase_2.72.0              GenomicRanges_1.64.0       
#>  [7] Seqinfo_1.2.0               IRanges_2.46.0             
#>  [9] S4Vectors_0.50.1            MatrixGenerics_1.24.0      
#> [11] matrixStats_1.5.0           BiocGenerics_0.58.1        
#> [13] generics_0.1.4             
#> 
#> loaded via a namespace (and not attached):
#>   [1] bitops_1.1-0                testthat_3.3.2             
#>   [3] rlang_1.3.0                 magrittr_2.0.5             
#>   [5] shinydashboard_0.7.3        otel_0.2.0                 
#>   [7] compiler_4.6.1              systemfonts_1.3.2          
#>   [9] vctrs_0.7.3                 reshape2_1.4.5             
#>  [11] relations_0.6-18            stringr_1.6.0              
#>  [13] pkgconfig_2.0.3             crayon_1.5.3               
#>  [15] fastmap_1.2.0               backports_1.5.1            
#>  [17] XVector_0.52.0              caTools_1.18.4             
#>  [19] promises_1.5.0              rmarkdown_2.31             
#>  [21] ragg_1.5.2                  coop_0.6-3                 
#>  [23] xfun_0.60                   MultiAssayExperiment_1.38.0
#>  [25] cachem_1.1.0                jsonlite_2.0.0             
#>  [27] SnowballC_0.7.1             later_1.4.8                
#>  [29] DelayedArray_0.38.2         BiocParallel_1.46.0        
#>  [31] parallel_4.6.1              sets_1.0-25                
#>  [33] cluster_2.1.8.2             R6_2.6.1                   
#>  [35] stringi_1.8.9               bslib_0.12.0               
#>  [37] RColorBrewer_1.1-3          limma_3.68.5               
#>  [39] boot_1.3-32                 brio_1.1.5                 
#>  [41] jquerylib_0.1.4             assertthat_0.2.1           
#>  [43] Rcpp_1.1.2                  knitr_1.51                 
#>  [45] downloader_0.4.1            httpuv_1.6.17              
#>  [47] Matrix_1.7-5                igraph_2.3.3               
#>  [49] tidyselect_1.2.1            abind_1.4-8                
#>  [51] yaml_2.3.12                 stringfish_0.19.2          
#>  [53] gplots_3.3.0                codetools_0.2-20           
#>  [55] plyr_1.8.9                  lattice_0.22-9             
#>  [57] tibble_3.3.1                withr_3.0.3                
#>  [59] shiny_1.14.0                BumpyMatrix_1.20.0         
#>  [61] S7_0.2.2                    evaluate_1.0.5             
#>  [63] desc_1.4.3                  RcppParallel_6.2.0         
#>  [65] bench_1.1.4                 pillar_1.11.1              
#>  [67] lsa_0.73.4                  KernSmooth_2.23-26         
#>  [69] checkmate_2.3.4             DT_0.34.0                  
#>  [71] shinyjs_2.1.1               piano_2.28.0               
#>  [73] ggplot2_4.0.3               scales_1.4.0               
#>  [75] gtools_3.9.5                xtable_1.8-8               
#>  [77] marray_1.90.0               qs2_0.2.2                  
#>  [79] glue_1.8.1                  slam_0.1-56                
#>  [81] tools_4.6.1                 data.table_1.18.4          
#>  [83] gDRutils_1.10.0             fgsea_1.38.0               
#>  [85] fs_2.1.0                    visNetwork_2.1.4           
#>  [87] fastmatch_1.1-8             cowplot_1.2.0              
#>  [89] grid_4.6.1                  cli_3.6.6                  
#>  [91] textshaping_1.0.5           S4Arrays_1.12.0            
#>  [93] dplyr_1.2.1                 gtable_0.3.6               
#>  [95] sass_0.4.10                 digest_0.6.39              
#>  [97] SparseArray_1.12.2          htmlwidgets_1.6.4          
#>  [99] farver_2.1.2                htmltools_0.5.9            
#> [101] pkgdown_2.2.1               lifecycle_1.0.5            
#> [103] statmod_1.5.2               mime_0.13
```
