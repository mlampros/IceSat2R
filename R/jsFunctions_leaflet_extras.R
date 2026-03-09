# ..............................................................
# reference: https://github.com/trafficonese/leaflet.extras
#
# "leaflet.extras" was archived at 2026-02-19 and is no
#  longer available on CRAN, thus I had to include the
#  required code snippets (I used previously) in this package
# ..............................................................

#' @importFrom htmlwidgets JS
#' @noRd
defIconFunction <-
  htmlwidgets::JS("function(icon){
        if (!$.isEmptyObject(icon)) {
          return L.icon(icon);
        } else {
          return L.icon();
        }
     }")

#' @importFrom htmlwidgets JS
#' @noRd
awesomeIconFunction <-
  htmlwidgets::JS("function(icon){
        if (!$.isEmptyObject(icon)) {
          if (!icon.prefix) {
            icon.prefix = icon.library;
          }
          return L.AwesomeMarkers.icon(icon);
        } else {
          return L.AwesomeMarkers.icon();
        }
     }")
