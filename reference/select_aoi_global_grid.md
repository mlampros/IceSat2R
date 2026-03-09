# R6 Class to Select an Area of Interest (AOI) from a Global Grid

The 'select_aoi_global_grid' class allows the user to select an Area of
Interest (AOI) (see the examples section for all available options)

## Usage

``` r
# init <- select_aoi_global_grid$new()
```

## References

https://github.com/r-spatial/mapedit/blob/master/experiments/gadget_draw2.R

## Public fields

- `area_of_interest`:

  an R object (character string, vector)

- `leaflet_provider_base`:

  a leaflet provider object

- `leaflet_provider_secondary`:

  a leaflet provider object

- `crs_value`:

  a numeric value

- `use_s2`:

  a boolean

- `verbose`:

  a boolean

## Methods

### Public methods

- [`select_aoi_global_grid$new()`](#method-select_aoi_global_grid-new)

- [`select_aoi_global_grid$draw_edit_internal()`](#method-select_aoi_global_grid-draw_edit_internal)

- [`select_aoi_global_grid$draw_edit_aoi()`](#method-select_aoi_global_grid-draw_edit_aoi)

- [`select_aoi_global_grid$selected_areas_global_grid()`](#method-select_aoi_global_grid-selected_areas_global_grid)

- [`select_aoi_global_grid$selected_aoi_sf()`](#method-select_aoi_global_grid-selected_aoi_sf)

- [`select_aoi_global_grid$clone()`](#method-select_aoi_global_grid-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialization method for the 'select_aoi_global_grid' R6 class

#### Usage

    select_aoi_global_grid$new(
      area_of_interest = NULL,
      leaflet_provider_base = leaflet::providers$CartoDB.Positron,
      leaflet_provider_secondary = leaflet::providers$Esri.WorldImagery,
      crs_value = 4326,
      use_s2 = FALSE,
      verbose = FALSE
    )

#### Arguments

- `area_of_interest`:

  either NULL (which allows the user to draw the area of interest on the
  map) or a character string (i.e. a 'country' or a 'continent') or a
  named bounding box vector (such as c(xmin = 16.1, xmax = 16.6, ymax =
  48.6, ymin = 47.9)). The 'countries' and 'continents' can be extracted
  from the "rnaturalearth::ne_countries(scale = 110, type = 'countries',
  returnclass = 'sf')" function and specifically the columns:
  'sovereignt' and 'continent'

- `leaflet_provider_base`:

  a leaflet provider object

- `leaflet_provider_secondary`:

  a leaflet provider object

- `crs_value`:

  a value. The coordinate reference system. The default value of the crs
  is 4326

- `use_s2`:

  a boolean. If TRUE, use the s2 spherical geometry package for
  geographical coordinate operations (see the documentation of the
  'sf::sf_use_s2()' function for more information)

- `verbose`:

  a boolean. If TRUE then information will be printed in the console

------------------------------------------------------------------------

### Method `draw_edit_internal()`

Internal Shiny application to visualize the selected area

#### Usage

    select_aoi_global_grid$draw_edit_internal(lft_map)

#### Arguments

- `lft_map`:

  a leaflet map

------------------------------------------------------------------------

### Method `draw_edit_aoi()`

Allows to view the Global Grid on the map including a popup that shows
the Area of each grid rectangle (or hexagon). The user can then select
an Area of Interest (AOI)

#### Usage

    select_aoi_global_grid$draw_edit_aoi(degrees = 1, square_geoms = TRUE)

#### Arguments

- `degrees`:

  a numeric value. This can be either 1.0 or 5.0 to allow queries to the
  'OpenAltimetry' API

- `square_geoms`:

  a boolean. If FALSE then a hexagonal grid will be created

------------------------------------------------------------------------

### Method `selected_areas_global_grid()`

Takes the selected Area(s) of Interest (AOI) from the draw_edit_aoi()
method and returns a simple features object with the corresponding
n-degree grid cells

#### Usage

    select_aoi_global_grid$selected_areas_global_grid(plot_data = FALSE)

#### Arguments

- `plot_data`:

  a boolean specifying if the selected from the user AOI's and the
  corresponding grid cells should be plotted side by side. If FALSE then
  only the simple features object will be returned. If TRUE and the
  initial 'area_of_interest' parameter is NULL then a single plot will
  be displayed.

#### Returns

either an 'sfc' object (if the initial 'area_of_interest' parameter is
NULL) or an 'sf' object consisting of the n-degree grid cells

------------------------------------------------------------------------

### Method `selected_aoi_sf()`

Returns the selected area of interest (AOI) by the user in form of an
'sfc' object

#### Usage

    select_aoi_global_grid$selected_aoi_sf()

#### Returns

an 'sfc' object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    select_aoi_global_grid$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

require(IceSat2R)
require(magrittr)

#........................................
# 1st. Option: Select an AOI from the map
#........................................

init = select_aoi_global_grid$new(area_of_interest = NULL,
                                  verbose = TRUE)
init$draw_edit_aoi()
sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
sf_obj


#...............................................
# observe the available countries and continents
# using the 'rnaturalearth' R package
#...............................................

cntr_cnt = rnaturalearth::ne_countries(scale = 110,
                                       type = 'countries',
                                       returnclass = 'sf')

cntr_cnt = cntr_cnt[, c('sovereignt', 'continent')]

# sort(cntr_cnt$sovereignt)
# sort(unique(cntr_cnt$continent))


#.......................................
# 2nd. Option: Select a 'country' as AOI   (5-degrees query)
#.......................................

init = select_aoi_global_grid$new(area_of_interest = 'Antarctica',
                                  verbose = TRUE)

init$draw_edit_aoi(degrees = 5.0, square_geoms = TRUE)
sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
sf_obj


#.........................................
# 3rd. Option: Select a 'continent' as AOI   (1-degree query)
#.........................................

init = select_aoi_global_grid$new(area_of_interest = 'North America',
                                  verbose = TRUE)

init$draw_edit_aoi(degrees = 1.0, square_geoms = TRUE)
sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
sf_obj


#.........................................
# 4th. Option: Use a bounding box as input  ('Greenland Ice Sheet')
#.........................................

data(ne_10m_glaciated_areas)

dat_bbx = ne_10m_glaciated_areas %>%
  subset(!is.na(name)) %>%
  subset(name == "Greenland Ice Sheet") %>%
  sf::st_bbox(crs = 4326)

dat_bbx

init = select_aoi_global_grid$new(area_of_interest = dat_bbx,
                                  verbose = TRUE)

init$draw_edit_aoi(degrees = 1.0, square_geoms = TRUE)
sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
sf_obj


#......................................................
# 5th. Option: Create a global hexagonal 5-degrees grid
#......................................................

bbx_global = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)

init = select_aoi_global_grid$new(area_of_interest = bbx_global,
                                  verbose = TRUE)

init$draw_edit_aoi(degrees = 5.0, square_geoms = FALSE)
sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
sf_obj

} # }
```
