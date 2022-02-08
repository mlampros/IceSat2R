

#' a leaflet base map
#'
#' @param sf_obj a simple features object which must include the columns 'date' and 'size_MB' if the popup parameter is set to TRUE
#' @param leaflet_provider_base a leaflet provider object
#' @param leaflet_provider_secondary a leaflet provider object
#' @param overlay_group a character string specifying the overlay group
#' @param overlay_polylines a character string specifying the Polylines group
#' @param width a numeric value specifying the width of the leaflet map
#' @param height a numeric value specifying the height of the leaflet map
#' @param popup a boolean specifying if the popup should be shown or not
#' @param collapse_layer_control a boolean specifying if the layer-control should be collapsed or not
#'
#' @return a leaflet object (map)
#'
#' @import magrittr
#' @importFrom glue glue
#' @importFrom sf st_cast st_geometry
#' @importFrom leaflet leaflet addProviderTiles addLayersControl layersControlOptions
#' @importFrom leafgl addGlPolylines addGlPolygons
#'
#' @keywords internal

leaflet_base_map = function(sf_obj = NULL,
                            leaflet_provider_base,
                            leaflet_provider_secondary,
                            overlay_group = 'IceSat2 1-degree',
                            overlay_polylines = 'Polylines',
                            width = NULL,
                            height = NULL,
                            popup = NULL,
                            collapse_layer_control = TRUE) {

  lft = leaflet::leaflet(width = width, height = height) %>%

    leaflet::addProviderTiles(provider = leaflet_provider_base,
                                  group = leaflet_provider_base) %>%

    leaflet::addProviderTiles(provider = leaflet_provider_secondary,
                                  group = leaflet_provider_secondary)

  overlay_groups = character(0)

  if (!is.null(sf_obj)) {

    if (!is.null(popup)) popup = glue::glue("area (km^2): {round(x = as.numeric(sf_obj[[popup]]), digits = 2)}")

    #.............................................................................................
    # Using a 'LINESTRING' is a work-around to display the boundaries of the polygons using leafGL
    # see: https://github.com/r-spatial/leafgl/issues/3#issuecomment-635375213
    #      https://github.com/r-spatial/leafgl/issues/68#issuecomment-1005605056
    #.............................................................................................
    pl_ln_dat = sf::st_cast(sf::st_geometry(sf_obj), 'LINESTRING')

    lft = leafgl::addGlPolylines(map = lft,
                                 data = pl_ln_dat,
                                 color = 'blue',
                                 opacity = 1.0,
                                 popup = NULL,
                                 group = overlay_polylines) %>%

      leafgl::addGlPolygons(data = sf_obj,
                            fillColor = 'green',
                            fillOpacity = 0.0,
                            popup = popup,
                            group = overlay_group)

    overlay_groups = c(overlay_polylines, overlay_group)
  }

  lft = leaflet::addLayersControl(map = lft,
                                  baseGroups = c(leaflet_provider_base, leaflet_provider_secondary),
                                  overlayGroups = overlay_groups,
                                  options = leaflet::layersControlOptions(collapsed = collapse_layer_control),
                                  position = "topleft")
  return(lft)
}


#' R6 Class to Select an Area of Interest (AOI) from a Global Grid
#'
#' @description
#' The 'select_aoi_global_grid' class will be used to select an Area of Interest (AOI) from the global grid
#'
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions
#' @importFrom sf st_as_sfc st_intersects sf_use_s2 st_bbox st_crs
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar miniTitleBarButton
#' @importFrom shiny observeEvent stopApp runGadget dialogViewer isTruthy
#' @importFrom rnaturalearth ne_countries
#' @importFrom leaflet providers
#' @importFrom leafsync sync
#' @importFrom glue glue
#'
#' @export
#' @docType class
#' @usage # init <- select_aoi_global_grid$new()
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#' require(magrittr)
#'
#' #........................................
#' # 1st. Option: Select an AOI from the map
#' #........................................
#'
#' init = select_aoi_global_grid$new(area_of_interest = NULL,
#'                                   verbose = TRUE)
#' init$draw_edit_aoi()
#' sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
#' sf_obj
#'
#'
#' #...............................................
#' # observe the available countries and continents
#' # using the 'rnaturalearth' R package
#' #...............................................
#'
#' cntr_cnt = rnaturalearth::ne_countries(scale = 110,
#'                                        type = 'countries',
#'                                        returnclass = 'sf')
#'
#' cntr_cnt = cntr_cnt[, c('sovereignt', 'continent')]
#'
#' # sort(cntr_cnt$sovereignt)
#' # sort(unique(cntr_cnt$continent))
#'
#'
#' #.......................................
#' # 2nd. Option: Select a 'country' as AOI   (5-degrees query)
#' #.......................................
#'
#' init = select_aoi_global_grid$new(area_of_interest = 'Antarctica',
#'                                   verbose = TRUE)
#'
#' init$draw_edit_aoi(degrees = 5.0, square_geoms = TRUE)
#' sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
#' sf_obj
#'
#'
#' #.........................................
#' # 3rd. Option: Select a 'continent' as AOI   (1-degree query)
#' #.........................................
#'
#' init = select_aoi_global_grid$new(area_of_interest = 'North America',
#'                                   verbose = TRUE)
#'
#' init$draw_edit_aoi(degrees = 1.0, square_geoms = TRUE)
#' sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
#' sf_obj
#'
#'
#' #.........................................
#' # 4th. Option: Use a bounding box as input  ('Greenland Ice Sheet')
#' #.........................................
#'
#' pth_bbx = system.file('data_files', 'ne_10m_glaciated_areas.RDS',
#'                       package = "IceSat2R")
#'
#' dat_bbx = readRDS(file = pth_bbx) %>%
#'   subset(!is.na(name)) %>%
#'   subset(name == "Greenland Ice Sheet") %>%
#'   sf::st_bbox(crs = 4326)
#'
#' dat_bbx
#'
#' init = select_aoi_global_grid$new(area_of_interest = dat_bbx,
#'                                   verbose = TRUE)
#'
#' init$draw_edit_aoi(degrees = 1.0, square_geoms = TRUE)
#' sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
#' sf_obj
#'
#'
#' #......................................................
#' # 5th. Option: Create a global hexagonal 5-degrees grid
#' #......................................................
#'
#' bbx_global = c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)
#'
#' init = select_aoi_global_grid$new(area_of_interest = bbx_global,
#'                                   verbose = TRUE)
#'
#' init$draw_edit_aoi(degrees = 5.0, square_geoms = FALSE)
#' sf_obj = init$selected_areas_global_grid(plot_data = TRUE)
#' sf_obj
#'
#' }


select_aoi_global_grid <- R6::R6Class("select_aoi_global_grid",

                                     public = list(

                                       #' @field area_of_interest an R object (character string, vector)
                                       #' @field leaflet_provider_base a leaflet provider object
                                       #' @field leaflet_provider_secondary a leaflet provider object
                                       #' @field crs_value a numeric value
                                       #' @field use_s2 a boolean
                                       #' @field verbose a boolean
                                       area_of_interest = NULL,
                                       leaflet_provider_base = NULL,
                                       leaflet_provider_secondary = NULL,
                                       crs_value = NULL,
                                       use_s2 = FALSE,
                                       verbose = FALSE,

                                       #' @description
                                       #' Initialize method for the select_aoi_global_grid R6 class
                                       #' @param area_of_interest either NULL (which allows the user to draw the area of interest on the map) or a character string (i.e. a 'country' or a 'continent') or a named bounding box vector (such as c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9)). The 'countries' and 'continents' can be extracted from the "rnaturalearth::ne_countries(scale = 110, type = 'countries', returnclass = 'sf')" function and specifically the columns: 'sovereignt' and 'continent'
                                       #' @param leaflet_provider_base a leaflet provider object
                                       #' @param leaflet_provider_secondary a leaflet provider object
                                       #' @param crs_value a value. The coordinate reference system. The default value of the crs is 4326
                                       #' @param use_s2 a boolean. If TRUE, use the s2 spherical geometry package for geographical coordinate operations (see the documentation of the 'sf::sf_use_s2()' function for more information)
                                       #' @param verbose a boolean. If TRUE then information will be printed in the console

                                       initialize = function(area_of_interest = NULL,
                                                             leaflet_provider_base = leaflet::providers$CartoDB.Positron,
                                                             leaflet_provider_secondary = leaflet::providers$Esri.WorldImagery,
                                                             crs_value = 4326,
                                                             use_s2 = FALSE,
                                                             verbose = FALSE) {

                                         sf::sf_use_s2(use_s2 = use_s2)             # do not use S2 (by default), because for EPSG:4326 can give an error especially when computing the 'intersection()' of 'sf' objects

                                         self$area_of_interest <- area_of_interest
                                         self$leaflet_provider_base <- leaflet_provider_base
                                         self$leaflet_provider_secondary <- leaflet_provider_secondary
                                         self$crs_value <- crs_value
                                         self$verbose <- verbose

                                         if (!is.null(self$area_of_interest)) {
                                           if (inherits(self$area_of_interest, 'character') & length(self$area_of_interest) == 1) {
                                             private$ne_cntr = rnaturalearth::ne_countries(scale = 110,
                                                                                           type = 'countries',
                                                                                           returnclass = 'sf')
                                             private$ne_cntr = private$ne_cntr[, c('sovereignt', 'continent')]
                                           }
                                         }
                                       },


                                       #' @description
                                       #' Internal Shiny application to visualize the global grid
                                       #'
                                       #' @param lft_map a leaflet map
                                       #'
                                       #' @references
                                       #'
                                       #' https://github.com/r-spatial/mapedit/blob/master/experiments/gadget_draw2.R

                                       draw_edit_internal = function(lft_map) {

                                         ui <- miniUI::miniPage(
                                           miniUI::miniContentPanel(lft_map),
                                           miniUI::gadgetTitleBar("Draw a Polygon to Select Grid Cells", right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE))
                                         )

                                         server <- function(input, output, session) {
                                           draw_obj <- list()
                                           edited <- list()

                                           shiny::observeEvent(input$undefined_draw_new_feature, {                           # 'draw' new feature
                                             draw_obj <<- c(draw_obj, list(input$undefined_draw_new_feature))
                                           })

                                           shiny::observeEvent(input$undefined_draw_edited_features, {                       # 'edit' existing feature
                                             edited <<- input$undefined_draw_edited_features
                                             ids <- unlist(lapply(draw_obj, function(x) {
                                               x$properties$`_leaflet_id`
                                             }))

                                             lapply(edited$features, function(x){
                                               loc <- match(x$properties$`_leaflet_id`, ids)
                                               draw_obj[loc] <<- list(x)
                                             })
                                           })

                                           shiny::observeEvent(input$undefined_draw_deleted_features, {                       # 'delete' all existing features (and return or re-draw)

                                             if (shiny::isTruthy(input$undefined_draw_deleted_features)) {
                                               draw_obj <<- list()
                                             }
                                           })

                                           shiny::observeEvent(input$done, {
                                             shiny::stopApp(returnValue = draw_obj)
                                           })
                                                  # 'returnValue' is the value that should be returned from the 'shiny::runGadget'
                                           shiny::observeEvent(input$cancel, {
                                             shiny::stopApp (returnValue = NULL)
                                           })

                                           # session$onSessionEnded(function() {
                                           #   shiny::stopApp()
                                           # })
                                         }

                                         shiny::runGadget(
                                           ui,
                                           server,
                                           viewer =  shiny::dialogViewer("Draw and Edit the Global Grid of Polygons"),
                                           stopOnCancel = FALSE
                                         )
                                       },


                                       #' @description
                                       #' Allows to view the Global Grid on the map including a popup that shows the Area of each grid rectangle (or hexagon). The user can then select an Area of Interest (AOI)
                                       #'
                                       #' @param degrees a numeric value. This can be either 1.0 or 5.0 to allow queries to the 'OpenAltimetry' API
                                       #' @param square_geoms a boolean. If FALSE then a hexagonal grid will be created

                                       draw_edit_aoi = function(degrees = 1.0, square_geoms = TRUE) {

                                         # if (!degrees %in% c(1.0, 5.0)) stop("The 'OpenAltimetry' API allows up to 1-degree bounding box queries for 'atl03' and up to 5-degree bounding box queries for the 'atl06', 'atl07', 'atl08', 'atl10', 'atl12' and 'atl13' products!", call. = F)      # throwing an error is too strict, because it is possible that the user might want to create a bigger-sized grid
                                         if (!degrees %in% c(1.0, 5.0)) message("The 'OpenAltimetry' API allows up to 1-degree bounding box queries for 'atl03' and up to 5-degree bounding box queries for the 'atl06', 'atl07', 'atl08', 'atl10', 'atl12' and 'atl13' products!")

                                         if (!is.null(self$area_of_interest)) {
                                           if (inherits(self$area_of_interest, 'character') & length(self$area_of_interest) == 1) {

                                             flag_invalid = TRUE
                                             if (self$area_of_interest %in% private$ne_cntr$sovereignt) {
                                               dat_aoi = subset(private$ne_cntr, sovereignt == self$area_of_interest)
                                               dat_bbx = sf::st_bbox(dat_aoi)
                                               flag_invalid = FALSE
                                             }
                                             else {
                                               message(glue::glue("The 'area_of_interest' parameter [ '{self$area_of_interest}' ] is not one of the 'rnaturalearth' countries!"))
                                             }

                                             if (self$area_of_interest %in% private$ne_cntr$continent) {
                                               dat_aoi = subset(private$ne_cntr, continent == self$area_of_interest)
                                               dat_bbx = sf::st_bbox(dat_aoi)
                                               flag_invalid = FALSE
                                             }
                                             else {
                                               message(glue::glue("The 'area_of_interest' parameter [ '{self$area_of_interest}' ] is not one of the 'rnaturalearth' continents!"))
                                             }

                                             if (flag_invalid) {
                                               message("An invalid 'country' or 'continent' was used as input! The default leaflet map will be displayed!")
                                               self$area_of_interest <- NULL
                                             }
                                           }
                                           else if ((inherits(self$area_of_interest, 'numeric') & all(names(self$area_of_interest) %in% c("xmin", "xmax", "ymax", "ymin"))) | inherits(self$area_of_interest, "bbox")) {
                                             dat_bbx = sf::st_bbox(obj = self$area_of_interest, crs = sf::st_crs(self$crs_value))
                                             dat_aoi = sf::st_as_sfc(dat_bbx)
                                           }
                                           else {
                                             stop("Input to the 'area_of_interest' parameter must be either a character string (country, continent) or a numeric named vector (bounding box consisting of 'xmin', 'xmax', 'ymin', 'ymax')!", call. = F)
                                           }
                                         }
                                         else {
                                           dat_bbx = NULL
                                           dat_aoi = NULL
                                         }

                                         if (!is.null(dat_bbx) & !is.null(dat_aoi)) {

                                           if (self$verbose) cat("The global grid map will be computed ...\n")
                                           dat_aoi_grid = degrees_to_global_grid(minx = as.numeric(dat_bbx['xmin']),
                                                                                 maxx = as.numeric(dat_bbx['xmax']),
                                                                                 maxy = as.numeric(dat_bbx['ymax']),
                                                                                 miny = as.numeric(dat_bbx['ymin']),
                                                                                 degrees = degrees,
                                                                                 square_geoms = square_geoms,
                                                                                 crs_value = self$crs_value,
                                                                                 verbose = self$verbose)

                                           idx_inters = suppressMessages(sf::st_intersects(x = dat_aoi, y = dat_aoi_grid, sparse = T))
                                           idx_inters = data.frame(idx_inters)
                                           if (nrow(idx_inters) == 0) {
                                             stop(glue::glue("The bounding box ('xmin', 'xmax', 'ymin', 'ymax') does not intersect with the computed global grid of '{degrees}' degrees!"), call. = F)
                                           }
                                           else {
                                             private$dat_aoi = dat_aoi_grid[idx_inters$col.id, , drop = FALSE]
                                           }
                                         }
                                         else {
                                           private$dat_aoi = NULL
                                         }

                                         if (self$verbose) cat("The base leaflet map will be created ...\n")
                                         lft = leaflet_base_map(sf_obj = private$dat_aoi,
                                                                leaflet_provider_base = self$leaflet_provider_base,
                                                                leaflet_provider_secondary = self$leaflet_provider_secondary,
                                                                width = 570,                                                          # by default use 570 pixels in width and height
                                                                height = 570,
                                                                popup = NULL)                                                         # !! don't use a pop-up because I have to select an area on the map and this will also open the pop-up window

                                         if (self$verbose) cat("Add the draw toolbar ...\n")
                                         lft = leaflet.extras::addDrawToolbar(map = lft,
                                                                              position = 'topleft',
                                                                              polylineOptions = FALSE,                                # disable everything except for the Polygons
                                                                              circleOptions = FALSE,
                                                                              markerOptions = FALSE,
                                                                              circleMarkerOptions = FALSE,
                                                                              editOptions = leaflet.extras::editToolbarOptions())

                                         if (self$verbose) cat("The draw & edit toolbar will be opened ...\n")
                                         private$draw_obj = list()
                                         private$draw_obj = tryCatch(select_aoi_global_grid$public_methods$draw_edit_internal(lft_map = lft), error = function(e) e)

                                         if (inherits(private$draw_obj, 'error')) stop(glue::glue("The 'draw_edit_aoi()' function raised the following error: '{private$draw_obj$message}' !"), call. = F)
                                       },


                                       #' @description
                                       #' Takes the selected Area(s) of Interest (AOI) from the draw_edit_aoi() method and returns a simple features object with the corresponding n-degree grid cells
                                       #'
                                       #' @param plot_data a boolean specifying if the selected from the user AOI's and the corresponding grid cells should be plotted side by side. If FALSE then only the simple features object will be returned. If TRUE and the initial 'area_of_interest' parameter is NULL then a single plot will be displayed.
                                       #'
                                       #' @return either an 'sfc' object (if the initial 'area_of_interest' parameter is NULL) or an 'sf' object consisting of the n-degree grid cells

                                       selected_areas_global_grid = function(plot_data = FALSE) {

                                         if (inherits(private$draw_obj, 'list')) {
                                           if (length(private$draw_obj) == 0) {                       # if I 'delete' the existing geometries I expect this to return an empty list
                                             msg = "The edited map did not return any geometries!"
                                             if (self$verbose) message(msg)
                                             return(msg)
                                           }
                                           else {

                                             if (self$verbose) cat("The Polygons & MultiPolygons will be extracted ...\n")
                                             draw_obj_sf = unlist(lapply(private$draw_obj, function(x) {
                                               paste(c("POLYGON ((", paste(unlist(lapply(x$geometry$coordinates[[1]], function(y) paste(y, collapse = ' '))), collapse = ', '), "))"), collapse = "")
                                             }))

                                             if (self$verbose) cat("Conversion to Simple Features ...\n")
                                             private$aoi_sf = sf::st_as_sfc(draw_obj_sf, crs = self$crs_value)

                                             if (plot_data) {
                                               if (self$verbose) cat("The Leaflet Map of the 'selected areas' will be computed ...\n")
                                               lft_first = leaflet_base_map(sf_obj = private$aoi_sf,
                                                                            leaflet_provider_base = self$leaflet_provider_base,
                                                                            leaflet_provider_secondary = self$leaflet_provider_secondary,
                                                                            overlay_group = NULL,                                            # !! set this group to NULL for the 'sync' map to work
                                                                            overlay_polylines = 'User selected AOI',
                                                                            width = 750,
                                                                            height = 750,
                                                                            popup = NULL,                           # !! don't use a pop-up because I don't have the 'area' column
                                                                            collapse_layer_control = FALSE)
                                             }

                                             if (is.null(private$dat_aoi)) {
                                               if (self$verbose) cat("The simple map-edit functionality was used! Grid cell geometries won't be returned!\n")
                                               if (plot_data) print(lft_first)
                                               return(private$aoi_sf)
                                             }
                                             else {

                                               if (self$verbose) cat("Computation of the intersection between the selected areas and the global Grid ...\n")
                                               inters = sf::st_intersects(private$aoi_sf, private$dat_aoi, sparse = TRUE)
                                               inters = data.frame(inters)
                                               if (nrow(inters) == 0) stop("I expect that there is an intersection between the selected area and the global grid!", call. = F)

                                               if (self$verbose) cat("The relevant polygons will be extracted ...\n")
                                               dat_products = private$dat_aoi[inters$col.id, , drop = FALSE]

                                               if (self$verbose) cat(glue::glue("Number of grid cells: {nrow(dat_products)}  Total km^2 area (all grid cells): {round(x = sum(dat_products$area, na.rm = T), digits = 2)}"), '\n')

                                               if (plot_data) {

                                                 if (self$verbose) cat("The Leaflet Map of the 'global grid cells' will be computed ...\n")
                                                 lft_second = leaflet_base_map(sf_obj = dat_products,
                                                                               leaflet_provider_base = self$leaflet_provider_base,
                                                                               leaflet_provider_secondary = self$leaflet_provider_secondary,
                                                                               overlay_group = NULL,                                           # !! set this group to NULL for the 'sync' map to work
                                                                               overlay_polylines = 'Grid Cell',
                                                                               width = 750,
                                                                               height = 750,
                                                                               popup = 'area',                                # use the 'area' column to display the area per grid cell
                                                                               collapse_layer_control = FALSE)

                                                 lft_sync = leafsync::sync(lft_first, lft_second)
                                                 print(lft_sync)
                                               }

                                               return(dat_products)
                                             }
                                           }
                                         }
                                         else {
                                           stop("I expect that the 'private$draw_obj' is of type list!", call. = F)
                                         }
                                       },


                                       #' @description
                                       #' Returns the selected area of interest (AOI) by the user in form of an 'sfc' object
                                       #'
                                       #' @return an 'sfc' object

                                       selected_aoi_sf = function() {
                                         if (is.null(private$aoi_sf)) stop("Please run the 'selected_areas_global_grid()' method first!", call. = F)
                                         return(private$aoi_sf)
                                       }

                                     ),

                                     private = list(
                                       dat_aoi = NULL,
                                       draw_obj = NULL,
                                       aoi_sf = NULL,
                                       ne_cntr = NULL
                                     )
)

