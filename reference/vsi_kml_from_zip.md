# Utilizing Virtual File Systems (vsi) to extract the .kml from the .zip file

This function returns the '.kml' and '.kmz' files in form of virtual
file paths. Moreover, the user has the option to download these files.

## Usage

``` r
vsi_kml_from_zip(
  icesat_rgt_url,
  download_zip = FALSE,
  download_method = "curl",
  verbose = FALSE
)
```

## Arguments

- icesat_rgt_url:

  a character string specifying the input .zip URL

- download_zip:

  a boolean. If TRUE the .zip file will be first downloaded and then the
  .kml files will be returned, otherwise the 'gdalinfo' function will be
  used as input to the R 'system2()' function to read the .kml files
  without downloading the .zip file. The 'gdalinfo' command requires
  that the user has configured GDAL properly.

- download_method:

  a character string specifying the download method. Corresponds to the
  'method' parameter of the 'utils::download.file()' function. Can be
  one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl'
  or 'auto'

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

an one column data.table of the output files

## References

https://icesat-2.gsfc.nasa.gov/science/specs

https://gdal.org/user/virtual_file_systems.html

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

URL = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/arcticallorbits.zip'

#..................................
# without downloading the .zip file
#..................................

res_out = vsi_kml_from_zip(icesat_rgt_url = URL,
                           download_zip = FALSE,
                           download_method = 'curl',
                           verbose = TRUE)
str(res_out)


#.............................
# by downloading the .zip file
#.............................

res_out = vsi_kml_from_zip(icesat_rgt_url = URL,
                           download_zip = TRUE,
                           download_method = 'curl',
                           verbose = TRUE)
str(res_out)

} # }
```
