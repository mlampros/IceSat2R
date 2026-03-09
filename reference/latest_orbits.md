# Extraction of the url from the Technical Specification Website

This function allows the user to view the latest 'Nominal' and 'Time
Specfic' orbit metadata (Url, Reference Ground Track Names, Dates and
Types)

## Usage

``` r
latest_orbits(
  technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
  verbose = FALSE
)
```

## Arguments

- technical_specs_url:

  a character string specifying the technical specs website

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

a 'data.table' object

## References

https://icesat-2.gsfc.nasa.gov/science/specs

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

orbs = latest_orbits(verbose = TRUE)
orbs

} # }
```
