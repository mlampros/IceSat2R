# Reference Ground Tracks (RGTs)

This function returns the url's of all Reference Ground Track (RGT)
cycles. Moreover, it returns the dates that belong to each RGT cycle and
the names of the RGT cycles.

## Usage

``` r
available_RGTs(
  only_cycle_names = FALSE,
  technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
  verbose = FALSE
)
```

## Arguments

- only_cycle_names:

  a boolean. If TRUE then only the RGT (Reference Ground Track) cycle
  names will be returned. Otherwise all orbit files, dates and cycle
  names.

- technical_specs_url:

  a character string specifying the technical specs website

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

a list object with the available orbit files, dates and cycle names

## References

https://icesat-2.gsfc.nasa.gov/science/specs

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

# .................................................
# all available orbit files, dates and cycle names
# .................................................

avail_dat <- available_RGTs(
  only_cycle_names = FALSE,
  verbose = TRUE
)
avail_dat

# .............................
# receive only the cycle names
# .............................

avail_cycles <- available_RGTs(
  only_cycle_names = TRUE,
  verbose = TRUE
)
avail_cycles
} # }
```
