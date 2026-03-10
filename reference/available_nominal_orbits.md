# Nominal mission orbits

This function allows the user to view the nominal orbits (all or a
selection)

## Usage

``` r
available_nominal_orbits(
  orbit_area = NULL,
  technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
  verbose = FALSE
)
```

## Arguments

- orbit_area:

  either NULL or a character string specifying the earth partition to
  use, it can be one of 'antarctic', 'arctic', 'western_hemisphere' and
  'eastern_hemisphere'

- technical_specs_url:

  a character string specifying the technical specs website

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

a list object with the available nominal orbits

## References

https://icesat-2.gsfc.nasa.gov/science/specs

## Examples

``` r
require(IceSat2R)

# .............................
# all available nominal orbits
# .............................

nomin_orb <- available_nominal_orbits(verbose = TRUE)
#> The available Icesat-2 orbits will be red from 'https://icesat-2.gsfc.nasa.gov/science/specs' ... 
#> Access the data of the technical specs website ...
#> Extract the .zip files and the corresponding titles ...
#> Keep the relevant data from the url's and titles ...
#> Process the nominal and time specific orbits separately ...
#> Adjust the Dates of the time specific orbits ...
#> Create the nominal orbits data.table ...
#> Create the time specific orbits data.table ...
#> Return a single data.table ...
#> Elapsed time: 0 hours and 0 minutes and 0 seconds. 
nomin_orb
#> $antarctic
#> [1] "https://icesat-2.gsfc.nasa.gov//sites/default/files/page_files/antarcticaallorbits.zip"
#> 
#> $arctic
#> [1] "https://icesat-2.gsfc.nasa.gov//sites/default/files/page_files/arcticallorbits.zip"
#> 
#> $western_hemisphere
#> [1] "https://icesat-2.gsfc.nasa.gov//sites/default/files/page_files/ICESat2groundtracksWesternHem.zip"
#> 
#> $eastern_hemisphere
#> [1] "https://icesat-2.gsfc.nasa.gov//sites/default/files/page_files/ICESat2groundtracksEasternHem.zip"
#> 

# ........................
# specific nominal orbits
# ........................

nomin_orb <- available_nominal_orbits(
  orbit_area = "arctic",
  verbose = TRUE
)
#> The available Icesat-2 orbits will be red from 'https://icesat-2.gsfc.nasa.gov/science/specs' ... 
#> Access the data of the technical specs website ...
#> Extract the .zip files and the corresponding titles ...
#> Keep the relevant data from the url's and titles ...
#> Process the nominal and time specific orbits separately ...
#> Adjust the Dates of the time specific orbits ...
#> Create the nominal orbits data.table ...
#> Create the time specific orbits data.table ...
#> Return a single data.table ...
#> Elapsed time: 0 hours and 0 minutes and 0 seconds. 
nomin_orb
#> $arctic
#> [1] "https://icesat-2.gsfc.nasa.gov//sites/default/files/page_files/arcticallorbits.zip"
#> 
```
