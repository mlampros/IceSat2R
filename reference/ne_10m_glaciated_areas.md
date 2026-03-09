# Natural Earth 10m Glaciated Areas (1:10 million scale)

Polygons derived from DCW (Digital Chart of the World), except for
Antarctica derived from MOA. Includes name attributes for major polar
glaciers. Filtering has aggregated some minor glaciers and eliminated
others from the original DCW data.

## Usage

``` r
data(ne_10m_glaciated_areas)
```

## Format

An object of class `sf` (inherits from `data.frame`) with 68 rows and 6
columns.

## Details

Issues: Needs scale rank attributes

Note: The original DCW data was collected decades ago. Since then many
mountain glaciers have either disappeared or diminished in size. This
data theme is deliberately called "glaciated areas" instead of
"glaciers" to reflect the changeable character of glacier extents.

## References

https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-glaciated-areas/

## Examples

``` r
require(IceSat2R)
require(sf)
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

data(ne_10m_glaciated_areas)
```
