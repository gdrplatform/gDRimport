## 1.1.3 (2023-12-19)
- update package vignette

## 1.1.1 (2023-12-01)
- update validation function used during data submission

## 1.1.1 (2023-11-22)
- sync main with devel branch
- add "Treatment" as template identifier

## 1.1.0 (2023-10-24)
- release Bioc 3.18

## 1.0.0 (2023-10-24)
- prerelease Bioc 3.18

## 0.99.25 (2023-10-17)
- adjust NEWS to Bioc format

## 0.99.24 (2023-10-02)
- add functions & unit tests for converting gDR MAE to PSet

## 0.99.23 (2023-09-22)
- correct plate size calculation

## 0.99.22 (2023-09-20)
- set barcode as character in the manifest file

## 0.99.21 (2023-09-14)
- disable support for 'xls' file format due to crashes

## 0.99.20 (2023-08-25)
- refactor subsetting of data.table using colname

## 0.99.19 (2023-07-19)
- update warning messages

## 0.99.18 (2023-07-02)
- add BiocStyle

## 0.99.17 (2023-06-27)
- add exception entry for invalid average dose-response data

## 0.99.16 (2023-06-23)
- increase compression level (Tavor_2020.qs; < 5MB limit)

## 0.99.15 (2023-06-22)
- replaced rds with qs

## 0.99.14 (2023-06-13)
- switch from `merge` to `[[`

## 0.99.13 (2023-06-05)
- replaced reshape2 functions by functions from data.table

## 0.99.12 (2023-05-24)
- format the vignette with BiocStyle

## 0.99.11 (2023-05-16)
- data.frame => data.table switch (next round of changes)

## 0.99.10 (2023-05-04)
- switch from `tibble` to `data.table` in excel files

## 0.99.9 (2023-04-25)
- refactor tibble, data.frame --> data.table

## 0.99.8 (2023-04-20)
- switch to OSI license

## 0.99.7 (2023-04-20)
- clean-up vignette

## 0.99.6 (2023-04-19)
- add object S4 `gdr_test_data`

## 0.99.5 (2023-04-19)
- mocked PSets tests

## 0.99.4 (2023-04-17)
- add R 4.2 as dependency
- bugfix for Pset-related tests and examples (reset identifiers)

## 0.99.3 (2023-04-13)
- add minor improvements (BiocCheck compatibility)

## 0.99.2 (2023-04-11)
- add support for `PharmacoGx`

## 0.99.1 (2023-04-07)
- update maintainer

## 0.99.0 (2023-03-24)
- make the package Bioc-compatible

## 0.1.3.18 (2023-03-01)
- remove obsolete code

## 0.1.3.17 (2023-02-14)
- add tests for check_metadata_names

## 0.1.3.16 (2023-01-23)
- add function for detecting format of raw data

## 0.1.3.15 (2022-12-20)
- add exceptions for missing drugs' and cell lines' annotations

## 0.1.3.14 (2022-12-14)
- make exception tests more robust

## 0.1.3.13 (2022-12-13)
- add additional error message for handling data in `gDRin`

## 0.1.3.12 (2022-11-02)
- add exception table and getter and update exceptions in `load_files.R`

## 0.1.3.11 (2022-10-26)
- fix bug with reading two untreated templates

## 0.1.3.10 (2022-10-17)
- add missing namespace and add unit tests for `correct_template_sheets`

## 0.1.3.9 (2022-10-06)
- extend logic for correcting template data (II)

## 0.1.3.8 (2022-10-04)
- extend logic for correcting manifest and template data

## 0.1.3.7 (2022-09-12)
- extend the logic for filling empty cols in plate design

## 0.1.3.6 (2022-07-28)
- change the logic for identification plates using more versatile solution

## 0.1.3.5 (2022-07-27)
- add assert for templates without untreated controls

## 0.1.3.4 (2022-07-06)
- reduce warnings and get rid of apply()

## 0.1.3.3 (2022-06-17)
- modify the logic for detecting empty columns

## 0.1.3.2 (2022-06-15)
- extend the logic for loading EnVision data for other plate size

## 0.1.3.1 (2022-06-03)
- add support to load drug doses from D300 .tdd files

## 0.1.3.0 (2022-06-02)
- release

## 0.0.14 (2022-05-18)
- add load_results_Tecan
- add tests for load_results_Tecan

## 0.0.12 (2022-04-21)
- fix unit tests

## 0.0.11 (2022-03-30)
- add support for Plate as an equivalent of Barcode

## 0.0.10 (2022-03-24)
- fix hardcoded identifiers
- update error message for custom column names

## 0.0.9 (2021-09-10)
- switch from `get_identifier` to `get_env_identifiers`

## 0.0.8 (2021-08-24)
- fix wrong argument in `convert_se_assay_to_dt`

## 0.0.7 (2021-07-23)
- 's/assay_to_dt/convert_se_assay_to_dt/' in write_ref_data_se
- fix linter issues
- knitr as Suggested package

## 0.0.6 (2021-07-06)
- remove nested repo structure

## 0.0.5 (2021-06-28)
- restore functions for reading reference data

## 0.0.5 (2021-06-28)
- add unit tests

## 0.0.4 (2021-06-25)
- add exemplary EnVision data

## 0.0.3 (2021-06-25)
- move functions related to reading template from gDRcore package

## 0.0.2 (2021-06-25)
- code refactor using lintr

## 0.0.1 (2021-06-22)
- initial release ( importing fucntions moved from gDRcore package)
