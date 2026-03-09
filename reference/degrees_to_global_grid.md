# Create a global grid based on degrees

This function allows the user to create a degrees grid based on an input
bounding box

## Usage

``` r
degrees_to_global_grid(
  minx = -180,
  maxx = 180,
  maxy = 90,
  miny = -90,
  degrees = 1,
  square_geoms = TRUE,
  crs_value = 4326,
  verbose = FALSE
)
```

## Arguments

- minx:

  the 'minx' parameter of the bounding box

- maxx:

  the 'maxx' parameter of the bounding box

- maxy:

  the 'maxy' parameter of the bounding box

- miny:

  the 'miny' parameter of the bounding box

- degrees:

  a numeric value specifying the degrees. It defaults to 1.0

- square_geoms:

  a boolean. If FALSE then a hexagonal grid will be created

- crs_value:

  a value. The coordinate reference system of the output grid. The crs
  must correspond to the EPSG of the input 'minx', 'maxx', 'miny' and
  'maxy'. Moreover, the 'EPSG:4326' returns a degrees grid as output.

- verbose:

  a boolean. If TRUE then information will be printed out in the console

## Value

a simple features (sf) object

## Details

The default global input 'minx', 'maxx', 'miny' and 'maxy' correspond to
a WKT of "POLYGON ((-180 -90, 180 -90, 180 90, -180 90, -180 -90))" When
'minx', 'maxx', 'miny' and 'maxy' EPSG is lat,long (or 'EPSG:4326') I
expect the output grid to be in 'degrees' unit. Using an EPSG other than
'EPSG:4326' might return a different output unit (such as meters). See
also the following 'Stackoverflow' thread for more information,
https://stackoverflow.com/a/64903836/8302386

Based on 'Approximate Metric Equivalents' 1 degree is approximately 111
km (or 60 nautical miles), Reference:
https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)

#...........................................
# 'OpenAltimetry' allows 1x1 degree bounding
# box selection for the 'atl03' Product
#...........................................

gl_grid_1_d = degrees_to_global_grid(degrees = 1.0, verbose = TRUE)
gl_grid_1_d
# summary(gl_grid_1_d$area)

#...........................................................................
# 'OpenAltimetry' allows 5x5 degree bounding box selection for the following
# Products: 'atl06', 'atl07', 'atl08', 'atl10', 'atl12', 'atl13', 'level3a'
#...........................................................................

gl_grid_5_d = degrees_to_global_grid(degrees = 5.0, verbose = TRUE)
gl_grid_5_d

} # }
```
