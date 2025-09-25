## gDRimport 1.7.3 - 2025-09-25
* add support for Incucyte data

## gDRimport 1.7.2 - 2025-05-15
* add support for depmap public PRISM data

## gDRimport 1.7.1 - 2025-04-16
* synchronize Bioconductor and GitHub versioning

## gDRimport 1.5.11 - 2025-04-10
* replace with_mock with with_mocked_bindings

## gDRimport 1.5.10 - 2025-03-26
* unify support for using `OncotreeLineage` for PRISM level 5 and level 6

## gDRimport 1.5.9 - 2025-03-12
* add support for using `OncotreeLineage` as a tissue annotation if available in PRISM data

## gDRimport 1.5.8 - 2025-02-18
* update extracting Tissue information for cell lines in PRISM data

## gDRimport 1.5.7 - 2025-02-07
* extract Tissue information for cell lines in PRISM data

## gDRimport 1.5.6 - 2025-02-04
* update error messages, to make them less confusing

## gDRimport 1.5.5 - 2025-01-11
* take into account run in public PRISM data

## gDRimport 1.5.4 - 2024-12-09
* support Duration in old private PRISM data format

## gDRimport 1.5.3 - 2024-12-04
* support old private PRISM data format

## gDRimport 1.5.2 - 2024-11-06
* fix invalid moa for PRISM data

## gDRimport 1.5.1 - 2024-11-05
* synchronize Bioconductor and GitHub versioning

## gDRimport 1.3.5 - 2024-10-22
* fix test for `load_files`

## gDRimport 1.3.4 - 2024-08-29
* fix failing github page

## gDRimport 1.3.3 - 2024-08-12
* update function for importing public PRISM data

## gDRimport 1.3.2 - 2024-07-03
* fix a bug with incorrect recognition of input file types

## gDRimport 1.3.1 - 2024-05-27
* synchronize Bioconductor and GitHub versioning

## gDRimport 1.1.11 - 2024-05-21
* move functions for importing PRISM data

## gDRimport 1.1.10 - 2024-05-08
* fix typo

## gDRimport 1.1.9 - 2024-04-08
* change output of `get_exception_data` to data.table

## gDRimport 1.1.8 - 2024-03-26
* fix issue with reading tsv files

## gDRimport 1.1.7 - 2024-03-07
* clean up the package

## gDRimport 1.1.6 - 2024-02-26
* improve pkgdown site
  * improved references
  * valid NEWS.md

## gDRimport 1.1.5 - 2024-02-14
* make documentation compatible with pkdgdown

## gDRimport 1.1.4 - 2024-01-22
* add new description fields

## gDRimport 1.1.3 - 2023-12-19
* update package vignette

## gDRimport 1.1.2 - 2023-12-01
* update validation function used during data submission

## gDRimport 1.1.1 - 2023-11-22
* sync main with devel branch
* add "Treatment" as template identifier

## gDRimport 1.1.0 - 2023-10-24
* release Bioc 3.18

## gDRimport 1.0.0 - 2023-10-24
* prerelease Bioc 3.18

## gDRimport 0.99.25 - 2023-10-17
* adjust NEWS to Bioc format

## gDRimport 0.99.24 - 2023-10-02
* add functions & unit tests for converting gDR MAE to PSet

## gDRimport 0.99.23 - 2023-09-22
* correct plate size calculation

## gDRimport 0.99.22 - 2023-09-20
* set barcode as character in the manifest file

## gDRimport 0.99.21 - 2023-09-14
* disable support for 'xls' file format due to crashes

## gDRimport 0.99.20 - 2023-08-25
* refactor subsetting of data.table using colname

## gDRimport 0.99.19 - 2023-07-19
* update warning messages

## gDRimport 0.99.18 - 2023-07-02
* add BiocStyle

## gDRimport 0.99.17 - 2023-06-27
* add exception entry for invalid average dose-response data

## gDRimport 0.99.16 - 2023-06-23
* increase compression level - Tavor_2020.qs; < 5MB limit

## gDRimport 0.99.15 - 2023-06-22
* replaced rds with qs

## gDRimport 0.99.14 - 2023-06-13
* switch from `merge` to `[[`

## gDRimport 0.99.13 - 2023-06-05
* replaced reshape2 functions by functions from data.table

## gDRimport 0.99.12 - 2023-05-24
* format the vignette with BiocStyle

## gDRimport 0.99.11 - 2023-05-16
* data.frame => data.table switch - next round of changes

## gDRimport 0.99.10 - 2023-05-04
* switch from `tibble` to `data.table` in excel files

## gDRimport 0.99.9 - 2023-04-25
* refactor tibble, data.frame --> data.table

## gDRimport 0.99.8 - 2023-04-20
* switch to OSI license

## gDRimport 0.99.7 - 2023-04-20
* clean-up vignette

## gDRimport 0.99.6 - 2023-04-19
* add object S4 `gdr_test_data`

## gDRimport 0.99.5 - 2023-04-19
* mocked PSets tests

## gDRimport 0.99.4 - 2023-04-17
* add R 4.2 as dependency
* bugfix for Pset-related tests and examples - reset identifiers

## gDRimport 0.99.3 - 2023-04-13
* add minor improvements - BiocCheck compatibility

## gDRimport 0.99.2 - 2023-04-11
* add support for `PharmacoGx`

## gDRimport 0.99.1 - 2023-04-07
* update maintainer

## gDRimport 0.99.0 - 2023-03-24
* make the package Bioc-compatible

## gDRimport 0.1.3.18 - 2023-03-01
* remove obsolete code

## gDRimport 0.1.3.17 - 2023-02-14
* add tests for check_metadata_names

## gDRimport 0.1.3.16 - 2023-01-23
* add function for detecting format of raw data

## gDRimport 0.1.3.15 - 2022-12-20
* add exceptions for missing drugs' and cell lines' annotations

## gDRimport 0.1.3.14 - 2022-12-14
* make exception tests more robust

## gDRimport 0.1.3.13 - 2022-12-13
* add additional error message for handling data in `gDRin`

## gDRimport 0.1.3.12 - 2022-11-02
* add exception table and getter and update exceptions in `load_files.R`

## gDRimport 0.1.3.11 - 2022-10-26
* fix bug with reading two untreated templates

## gDRimport 0.1.3.10 - 2022-10-17
* add missing namespace and add unit tests for `correct_template_sheets`

## gDRimport 0.1.3.9 - 2022-10-06
* extend logic for correcting template data - II

## gDRimport 0.1.3.8 - 2022-10-04
* extend logic for correcting manifest and template data

## gDRimport 0.1.3.7 - 2022-09-12
* extend the logic for filling empty cols in plate design

## gDRimport 0.1.3.6 - 2022-07-28
* change the logic for identification plates using more versatile solution

## gDRimport 0.1.3.5 - 2022-07-27
* add assert for templates without untreated controls

## gDRimport 0.1.3.4 - 2022-07-06
* reduce warnings and get rid of apply- 

## gDRimport 0.1.3.3 - 2022-06-17
* modify the logic for detecting empty columns

## gDRimport 0.1.3.2 - 2022-06-15
* extend the logic for loading EnVision data for other plate size

## gDRimport 0.1.3.1 - 2022-06-03
* add support to load drug doses from D300 .tdd files

## gDRimport 0.1.3.0 - 2022-06-02
* release

## gDRimport 0.0.14 - 2022-05-18
* add load_results_Tecan
* add tests for load_results_Tecan

## gDRimport 0.0.12 - 2022-04-21
* fix unit tests

## gDRimport 0.0.11 - 2022-03-30
* add support for Plate as an equivalent of Barcode

## gDRimport 0.0.10 - 2022-03-24
* fix hardcoded identifiers
* update error message for custom column names

## gDRimport 0.0.9 - 2021-09-10
* switch from `get_identifier` to `get_env_identifiers`

## gDRimport 0.0.8 - 2021-08-24
* fix wrong argument in `convert_se_assay_to_dt`

## gDRimport 0.0.7 - 2021-07-23
* 's/assay_to_dt/convert_se_assay_to_dt/' in write_ref_data_se
* fix linter issues
* knitr as Suggested package

## gDRimport 0.0.6 - 2021-07-06
* remove nested repo structure

## gDRimport 0.0.5 - 2021-06-28
* restore functions for reading reference data

## gDRimport 0.0.5 - 2021-06-28
* add unit tests

## gDRimport 0.0.4 - 2021-06-25
* add exemplary EnVision data

## gDRimport 0.0.3 - 2021-06-25
* move functions related to reading template from gDRcore package

## gDRimport 0.0.2 - 2021-06-25
* code refactor using lintr

## gDRimport 0.0.1 - 2021-06-22
* initial release -  importing fucntions moved from gDRcore package
