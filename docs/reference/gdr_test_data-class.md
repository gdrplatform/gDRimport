# gDR Test Data object

Object class `gdr_test_data` is build by function
[`get_test_data()`](https://gdrplatform.github.io/gDRimport/reference/get_test_data.md)

## Value

object class `gdr_test_data` with primary test data

## Slots

- `manifest_path`:

  character, path to manifest file

- `result_path`:

  character, path(s) to results file

- `template_path`:

  character, path(s) to data.table with template data

- `ref_m_df`:

  character, data.table with manifest data

- `ref_r1_r2`:

  character, path to reference file with raw data for treated &
  untreated

- `ref_r1`:

  character, path to reference file with raw data for treated

- `ref_t1_t2`:

  character, path to reference template file with treated & untreated
  data

- `ref_t1`:

  character, path to reference template file with treated data
