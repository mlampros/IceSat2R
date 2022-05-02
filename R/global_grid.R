
utils::globalVariables(c('km'))                        # for the conversion from meters to kilometers when using the 'units::set_units()' function


#' Create a global grid based on degrees
#'
#' This function allows the user to create a degrees grid based on an input bounding box
#'
#' @param minx the 'minx' parameter of the bounding box
#' @param maxx the 'maxx' parameter of the bounding box
#' @param miny the 'miny' parameter of the bounding box
#' @param maxy the 'maxy' parameter of the bounding box
#' @param degrees a numeric value specifying the degrees. It defaults to 1.0
#' @param square_geoms a boolean. If FALSE then a hexagonal grid will be created
#' @param crs_value a value. The coordinate reference system of the output grid. The crs must correspond to the EPSG of the input 'minx', 'maxx', 'miny' and 'maxy'. Moreover, the 'EPSG:4326' returns a degrees grid as output.
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return a simple features (sf) object
#'
#' @details
#'
#' The default global input 'minx', 'maxx', 'miny' and 'maxy' correspond to a WKT of "POLYGON ((-180 -90, 180 -90, 180 90, -180 90, -180 -90))"
#' When 'minx', 'maxx', 'miny' and 'maxy' EPSG is lat,long (or 'EPSG:4326') I expect the output grid to be in 'degrees' unit. Using an EPSG other than 'EPSG:4326' might
#' return a different output unit (such as meters). See also the following 'Stackoverflow' thread for more information, https://stackoverflow.com/a/64903836/8302386
#'
#' Based on 'Approximate Metric Equivalents' 1 degree is approximately 111 km  (or 60 nautical miles), Reference: https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
#'
#' @importFrom glue glue
#' @importFrom sf st_bbox st_as_sfc st_make_grid st_crs st_as_sf st_geometry st_area
#' @importFrom units set_units
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' #...........................................
#' # 'OpenAltimetry' allows 1x1 degree bounding
#' # box selection for the 'atl03' Product
#' #...........................................
#'
#' gl_grid_1_d = degrees_to_global_grid(degrees = 1.0, verbose = TRUE)
#' gl_grid_1_d
#' # summary(gl_grid_1_d$area)
#'
#' #...........................................................................
#' # 'OpenAltimetry' allows 5x5 degree bounding box selection for the following
#' # Products: 'atl06', 'atl07', 'atl08', 'atl10', 'atl12', 'atl13', 'level3a'
#' #...........................................................................
#'
#' gl_grid_5_d = degrees_to_global_grid(degrees = 5.0, verbose = TRUE)
#' gl_grid_5_d
#'
#' }

degrees_to_global_grid = function(minx = -180,
                                  maxx = 180,
                                  maxy = 90,
                                  miny = -90,
                                  degrees = 1.0,
                                  square_geoms = TRUE,
                                  crs_value = 4326,
                                  verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat('Create the bounding box and the sf object from the input minx, maxx, miny, maxy ...\n')
  }

  named_vec = c(xmin = minx, xmax = maxx, ymax = maxy, ymin = miny)
  glob_bounds_bbx = sf::st_bbox(obj = named_vec, crs = sf::st_crs(crs_value))
  glob_bounds = sf::st_as_sfc(glob_bounds_bbx)

  if (verbose) cat('Create the grid using the "sf::st_make_grid()" function ...\n')
  glob_grid = sf::st_make_grid(glob_bounds,
                               what = "polygons",
                               square = square_geoms,
                               cellsize = c(degrees, degrees),
                               flat_topped = FALSE,
                               crs = crs_value)
  update_name = 'geometry'
  glob_grid = sf::st_as_sf(glob_grid)
  names(glob_grid) = update_name                             # rename the geometry
  sf::st_geometry(glob_grid) = update_name                   # give the name 'geometry'

  if (verbose) cat('Compute the area in km^2 of the grid cells ...\n')
  glob_grid$area = sf::st_area(glob_grid)
  glob_grid$area = units::set_units(glob_grid$area, km^2)

  if (verbose) {
    cat(glue::glue('The output sf object consists of  {nrow(glob_grid)}  grid cells!'), '\n')
    compute_elapsed_time(time_start = t_start)
  }

  return(glob_grid)
}

