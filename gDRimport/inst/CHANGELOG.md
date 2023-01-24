<h3 align = "left"><strong>Changelog</strong></h3>

All notable changes to this project will be documented in this file.

#### [1.3.16] - 2023-01-23
- Add function for detecting format of raw data

#### [1.3.15] - 2022-12-20
- Add exceptions for missing drugs' and cell lines' annotations

#### [1.3.14] - 2022-12-14
- Make exception tests more robust

#### [1.3.13] - 2022-12-13
- Add additional error message for handling data in `gDRin`

#### [1.3.12] - 2022-11-02
- Add exception table and getter and update exceptions in `load_files.R`

#### [1.3.11] - 2022-10-26
- Fix bug with reading two untreated templates

#### [1.3.10] - 2022-10-17
- Add missing namespace and add unit tests for `correct_template_sheets`

#### [1.3.9] - 2022-10-06
- Extend logic for correcting template data (II)

#### [1.3.8] - 2022-10-04
- Extend logic for correcting manifest and template data

#### [1.3.7] - 2022-09-12
- Extend the logic for filling empty cols in plate design

#### [1.3.6] - 2022-07-28
- Change the logic for identification plates using more versatile solution

#### [1.3.5] - 2022-07-27
- Add assert for templates without untreated controls

#### [1.3.4] - 2022-07-06
- Reduce warnings and get rid of apply()

#### [1.3.3] - 2022-06-17
- Modify the logic for detecting empty columns

#### [1.3.2] - 2022-06-15
- Extend the logic for loading EnVision data for other plate size

#### [1.3.1] - 2022-06-03
- Add support to load drug doses from D300 .tdd files

#### [1.3.0] - 2022-06-02
- Release

#### [0.0.14] - 2022-05-18
- Add load_results_Tecan
- Add tests for load_results_Tecan

#### [0.0.12] - 2022-04-21
- Fix unit tests

#### [0.0.11] - 2022-03-30
- Add support for Plate as an equivalent of Barcode

#### [0.0.10] - 2022-03-24
- Fix hardcoded identifiers
- Update error message for custom column names

#### [0.0.9] - 2021-09-10
- switch from `get_identifier` to `get_env_identifiers`

#### [0.0.8] - 2021-08-24
- fix wrong argument in `convert_se_assay_to_dt`

#### [0.0.7] - 2021-07-23
- 's/assay_to_dt/convert_se_assay_to_dt/' in write_ref_data_se
- fix linter issues
- knitr as Suggested package

#### [0.0.6] - 2021-07-06
- remove nested repo structure

#### [0.0.5] - 2021-06-28
- restore functions for reading reference data

#### [0.0.5] - 2021-06-28
- add unit tests

#### [0.0.4] - 2021-06-25
- add exemplary EnVision data

#### [0.0.3] - 2021-06-25
- move functions related to reading template from gDRcore package

#### [0.0.2] - 2021-06-25
- code refactor using lintr

#### [0.0.1] - 2021-06-22
- initial release ( importing fucntions moved from gDRcore package)
