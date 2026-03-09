# Verification of the Reference Ground Tracks (RGTs)

This function allows the user to verify the NSIDC extracted RGTs with
the corresponding OpenAltimetry using the same Dates

## Usage

``` r
verify_RGTs(nsidc_rgts, bbx_aoi, verbose = FALSE, ...)
```

## Arguments

- nsidc_rgts:

  a data.frame, data.table or tibble object that includes the columns
  'Date_time' and 'RGT'

- bbx_aoi:

  a named numeric vector or an sf-bbox object with names 'xmin', 'ymin',
  'xmax', 'ymax'

- verbose:

  a boolean. If TRUE then information will be printed out in the console

- ...:

  further parameters for the getTracks function

## Value

a 'data.table' object where it is possible that the number of the
OpenAltimetry RGTs is higher compared to the NSIDC RGTs

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

rgts = data.table::setDT(list(RGT = c(1251L, 1252L, 1260L, 1267L, 1275L),
                              Date_time = c("2020-12-15", "2020-12-15",
                              "2020-12-15", "2020-12-16", "2020-12-16")))
bbx = c(xmin = -53.108876, ymin = 60.119614, xmax = -19.203521, ymax = 80.793117)

dtbl = verify_RGTs(nsidc_rgts = rgts, bbx_aoi = bbx, verbose = TRUE)
dtbl

# split by Date to observe RGTs by date

split(dtbl, by = 'Date_time')

} # }
```
