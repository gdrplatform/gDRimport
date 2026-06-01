# get exception data

get exception data

## Usage

``` r
get_exception_data(status_code = NULL)
```

## Arguments

- status_code:

  A numeric value

## Value

A data.table row with exception data or all exceptions

## Examples

``` r
get_exception_data(1)
#>    status_code                  title
#>         <char>                 <char>
#> 1:           1 Error loading manifest
#>                                                                    sprintf_text
#>                                                                          <char>
#> 1: There were errors loading manifest. Check error message below:\n```\n%s\n```
#>      type input_type
#>    <char>     <char>
#> 1:  error   manifest
get_exception_data()
#>     status_code                                    title
#>          <char>                                   <char>
#>  1:           1                   Error loading manifest
#>  2:           2                       Manifest not found
#>  3:           3                  Error loading treatment
#>  4:           4                Warning loading treatment
#>  5:           5                  Error loading treatment
#>  6:           6     Missing data for some Barcode, Plate
#>  7:           7     Missing data for some Barcode, Plate
#>  8:           8                   Error loading raw data
#>  9:           9                   Error loading raw data
#> 10:          10                Error during merging data
#> 11:          11                  Missing treatment files
#> 12:          12                   Error reading Manifest
#> 13:          13                     Manifest file format
#> 14:          14                        Manifest barcodes
#> 15:          15                       Treatment controls
#> 16:          16                          Instrument type
#> 17:          17                     Treatment sheet name
#> 18:          18                     Treatment entry name
#> 19:          19                  Error loading treatment
#> 20:          20         Error loading specific treatment
#> 21:          21          Error reading specific raw data
#> 22:          22 Error reading sheet in the raw data file
#> 23:          23      Error with readout values in plates
#> 24:          24             Error with treatment headers
#> 25:          25         Error with treatment file sheets
#> 26:          26              Metadata special characters
#> 27:          27                         Metadata invalid
#> 28:          28                       Raw Data delimiter
#> 29:          29                        Raw Data EnVision
#> 30:          30                      Raw Data Plate size
#> 31:          31                       Raw Data structure
#> 32:          32 Treatment File Gnumber and Concentration
#> 33:          33                Error loading input files
#> 34:          34                  Missing drug annotation
#> 35:          35             Missing cell line annotation
#> 36:          36                    Invalid averaged data
#> 37:          37                          Raw Data header
#>     status_code                                    title
#>          <char>                                   <char>
#>                                                                                                                                                                                                      sprintf_text
#>                                                                                                                                                                                                            <char>
#>  1:                                                                                                                                  There were errors loading manifest. Check error message below:\n```\n%s\n```
#>  2:                                                                                                                                                                                         Please load manifest!
#>  3:                                                         Some of the treatment files entered in the manifest have not been uploaded:\n%s\nPlease check the uploaded files and their name (including extension)
#>  4:                                                                                                                                                                     Some uploaded files (**%s**) are not used
#>  5:                                                                                                                       There were errors when loading the treatments. Check error message below:\n```\n%s\n```
#>  6:                                                                                                           Some plates (**%s**) found in the manifest are not reported in the data files and will be discarded
#>  7:                                                                                                           Some plates (**%s**) found in the data files are not reported in the manifest and will be discarded
#>  8:                                                                                                                         Txt file in an unsupported format. Please check if the file uses tab(\t) as separator
#>  9:                                                                                                                             There were errors when loading raw data. Check error message below:\n```\n%s\n```
#> 10:                                                                                                                                 There were errors when merging data. Check error message below:\n```\n%s\n```
#> 11:                                                                                                                                                                          Some treatment files are missing: %s
#> 12:                                                                                                                         There were errors reading the manifest file. Check error message below:\n```\n%s\n```
#> 13:                                                                                                                         %s file format is not supported. Please convert your file to one of the following: %s
#> 14:                                                                                                                                                                          Barcodes in Manifest must be unique!
#> 15:                                                                                                                   No untreated controls were found in the treatment. Please upload the appropriate treatment.
#> 16:                                                                                                                                                                                  Unrecognized instrument type
#> 17:                                                                                                                                                         In untreated treatment file %s, sheet name must be %s
#> 18:                                                                                                                                                            In untreated treatment file %s, entries must be %s
#> 19:                                                                                                                             There were errors loading the treatment. Check error message below:\n```\n%s\n```
#> 20:                                                                                                                                       There were errors loading treatment %s. Check error message below: \n%s
#> 21:                                                                                                                                                                                              Error reading %s
#> 22:                                                                                                                                                                                    Error reading %s, sheet %s
#> 23:                                                                                                                                        In result file %s (sheet %s) readout values are misplaced for plate %s
#> 24:                                                                    Treatment does not contain all expected headers for a '%s'. '%s' is/are required. Please correct your treatment or change the identifiers.
#> 25:                                                                                                                                  Treatment file %s does not contain %s sheets. Please correct your treatment.
#> 26:                                                                                                                      Metadata field names for %s cannot contain special characters or start with a number: %s
#> 27:                                                                                                                                              Metadata field name: %s in %s is not valid (reserved for output)
#> 28:                                                                                                                                                             Can't guess separator for the delimited files: %s
#> 29:                                                                                                                                                          Error reading %s: not an original EnVision .csv file
#> 30:                                                                                                                                                                            Error reading %s: wrong plate size
#> 31:                                                                                                                                       In result file %s (sheet %s) readout values are misplaced for plate %s.
#> 32:                           Treatment file(s) %s do/does not contain the same number of Gnumber_* and Concentration_* sheets. Gnumber_* and Concentration_* sheets are required. Please correct your treatment.
#> 33:                                                                                         Some input files are missing. Please upload only files with names starting with Manifest_*, Treatment_* and RawData_*
#> 34:                                                                                                                 The following drugs IDs are missing: '%s'. <br>Don't worry, drug names will be used for them.
#> 35:                                                                                                        The following cell line IDs are missing: '%s'. <br>Don't worry, cell line names will be used for them.
#> 36: Averaged dose-response data for the selected cell line and drug: '%s' can be considered invalid. Please check your data in the module 'Manage Data' or contact gdrplatform team via 'gdr-support-d@gene.com'.
#> 37:                                                                                                                                                                       Invalid header in the result file: (%s)
#>                                                                                                                                                                                                      sprintf_text
#>                                                                                                                                                                                                            <char>
#>        type   input_type
#>      <char>       <char>
#>  1:   error     manifest
#>  2:   error     manifest
#>  3:   error    treatment
#>  4: warning    treatment
#>  5:   error    treatment
#>  6: warning     manifest
#>  7: warning     manifest
#>  8:   error     raw data
#>  9:   error     raw data
#> 10:   error merging data
#> 11:   error    treatment
#> 12:   error     manifest
#> 13:   error     manifest
#> 14:   error     manifest
#> 15:   error    treatment
#> 16:   error     raw data
#> 17:   error    treatment
#> 18:   error    treatment
#> 19:   error    treatment
#> 20:   error    treatment
#> 21:   error     raw data
#> 22:   error     raw data
#> 23:   error     raw data
#> 24:   error    treatment
#> 25:   error    treatment
#> 26:   error     metadata
#> 27:   error     metadata
#> 28:   error     raw data
#> 29:   error     raw data
#> 30:   error     raw data
#> 31:   error     raw data
#> 32:   error    treatment
#> 33:   error  input files
#> 34: warning   submit tab
#> 35: warning   submit tab
#> 36:   error   assay data
#> 37:   error     raw data
#>        type   input_type
#>      <char>       <char>
```
