# Assert that suggested packages required by a function are installed

The PharmacoSet conversion functions are the only part of gDRimport that
needs CoreGx and PharmacoGx, and those two carry 37 further
dependencies, so they are suggested rather than imported. Call this
before touching their API.

## Usage

``` r
.assert_suggested_packages(pkgs, purpose)
```

## Arguments

- pkgs:

  character vector of package names.

- purpose:

  a short description of the functionality, used in the error message.

## Value

`NULL` invisibly.
