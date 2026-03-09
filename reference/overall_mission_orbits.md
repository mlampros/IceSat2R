# Overall Mission Orbits

This function allows the user to view information of the nominal mission
orbits and beam locations: "The processed files have 7 tracks per orbit:
one for each of the six beams of ICESat-2, and the seventh for the
Reference Ground Track (RGT). The RGT is an imaginary line through the
six-beam pattern that is handy for getting a sense of where the orbits
fall on Earth, and which the mission uses to point the observatory.
However, the six tracks for the six beams are our best estimate of where
the beams will actually fall on Earth's surface."

## Usage

``` r
overall_mission_orbits(
  orbit_area,
  download_method = "curl",
  threads = 1,
  verbose = FALSE
)
```

## Arguments

- orbit_area:

  a character string specifying the nominal mission orbits and beam
  locations. It can be one of 'antarctic', 'arctic',
  'western_hemisphere' or 'eastern_hemisphere'

- download_method:

  a character string specifying the download method. Corresponds to the
  'method' parameter of the 'utils::download.file()' function. Can be
  one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl'
  or 'auto'

- threads:

  an integer that specifies the number of threads to use in parallel
  when processing the data

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

an 'sf' object of multiple tracks (see the 'LAYER' column of the output
object)

## References

https://icesat-2.gsfc.nasa.gov/science/specs

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

res_orb  = overall_mission_orbits(orbit_area = 'antarctic',
                                  download_method = 'curl',
                                  threads = 1,
                                  verbose = TRUE)
str(res_orb)
} # }
```
