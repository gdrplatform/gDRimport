# Fix typos using reference data

Fix typos using reference data Evaluate given list of ids and try to
update them

## Usage

``` r
fix_typos_with_reference(
  data,
  ref,
  method = c("exact", "grepl", "adist"),
  fix_underscores = FALSE
)
```

## Arguments

- data:

  list of charvec(s) or charvec with data

- ref:

  charvec with reference data

- method:

  charvec type of the method to be used 'exact' is used to find
  identical entries from 'ref' in the data (after corrections and
  uppercase'ing) 'grepl' is used to find entries from 'ref' that might
  be somehow pre- or post- fixed

- fix_underscores:

  logical flag fix the issues with underscores in data identfiers?

## Value

list or charvec with corrected data
