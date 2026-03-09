# ..............................................................
# reference: https://github.com/trafficonese/leaflet.extras
#
# "leaflet.extras" was archived at 2026-02-19 and is no
#  longer available on CRAN, thus I had to include the
#  required code snippets (I used previously) in this package
# ..............................................................

#' Add Bootstrap dependency to a map
#' @param map the map widget
#' @importFrom leaflet leafletDependencies
#' @keywords internal
#' @noRd
addBootstrapDependency <- function(map) {
  map$dependencies <- c(
    map$dependencies,
    leaflet::leafletDependencies$bootstrap()
  )
  map
}

#' Add AwesomeMarkers and related lib dependencies to a map
#' @param map the map widget
#' @param libs char vector with lib names.
#' @importFrom leaflet leafletDependencies
#' @keywords internal
#' @noRd
addAwesomeMarkersDependencies <- function(map, libs) {
  map$dependencies <- c(
    map$dependencies,
    leaflet::leafletDependencies$awesomeMarkers()
  )
  if ("fa" %in% libs) {
    map$dependencies <- c(
      map$dependencies,
      leaflet::leafletDependencies$fontawesome()
    )
  }
  if ("ion" %in% libs) {
    map$dependencies <- c(
      map$dependencies,
      leaflet::leafletDependencies$ionicon()
    )
  }
  if ("glyphicon" %in% libs) {
    map$dependencies <- c(
      map$dependencies,
      leaflet::leafletDependencies$bootstrap()
    )
  }
  map
}

