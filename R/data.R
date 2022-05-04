

#' Natural Earth 10m Glaciated Areas (1:10 million scale)
#'
#' Polygons derived from DCW (Digital Chart of the World), except for Antarctica derived from MOA. Includes name attributes for major polar glaciers.
#' Filtering has aggregated some minor glaciers and eliminated others from the original DCW data.
#'
#' @references
#'
#' https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-glaciated-areas/
#'
#' @details
#'
#' Issues: Needs scale rank attributes
#'
#' Note: The original DCW data was collected decades ago. Since then many mountain glaciers have either disappeared or
#' diminished in size. This data theme is deliberately called "glaciated areas" instead of "glaciers" to reflect the
#' changeable character of glacier extents.
#'
#' @usage
#'
#' data(ne_10m_glaciated_areas)
#'
#' @keywords datasets
#'
#' @examples
#'
#' require(IceSat2R)
#' require(sf)
#'
#' data(ne_10m_glaciated_areas)
"ne_10m_glaciated_areas"



#' Reference Ground Tracks (RGTs) for IceSat-2 Cycle 14
#'
#' The data includes the following columns: "longitude", "latitude", "day_of_year", "Date", "hour", "minute", "second" and "RGT". The "RGT" column consists of
#' 1387 Reference Ground Tracks (RGTs) for the IceSat-2 Cycle 14 (from 'December 22, 2021' to 'March 23, 2022')
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @details
#'
#' The following code snippet shows how to come to the "RGT_cycle_14" data. The same can be done with any of the available RGT Cycles. For the following code
#' I utilized 8 threads to speed up the pre-processing of the downloaded .kml files (the code takes approximately 15 minutes on my Linux Personal Computer),
#'
#' \code{require(IceSat2R)} \cr
#' \code{require(magrittr)} \cr
#' \code{require(sf)} \cr
#'
#' \code{avail_cycles = available_RGTs(only_cycle_names = TRUE)} \cr
#' \code{avail_cycles} \cr
#'
#' \code{idx_cycle = 14} \cr
#'
#' \code{choose_cycle = avail_cycles[idx_cycle]} \cr
#' \code{choose_cycle} \cr
#'
#' \code{res_rgt_many = time_specific_orbits(RGT_cycle = choose_cycle, download_method = 'curl', threads = parallel::detectCores(), verbose = TRUE)} \cr
#'
#' \code{RGT_cycle_14 = sf::st_coordinates(res_rgt_many)} \cr
#' \code{colnames(RGT_cycle_14) = c('longitude', 'latitude')} \cr
#' \code{RGT_cycle_14 = data.table::data.table(RGT_cycle_14)} \cr
#' \code{RGT_cycle_14$day_of_year = res_rgt_many$day_of_year} \cr
#' \code{RGT_cycle_14$Date = as.Date(res_rgt_many$Date_time)} \cr
#' \code{RGT_cycle_14$hour = lubridate::hour(res_rgt_many$Date_time)} \cr
#' \code{RGT_cycle_14$minute = lubridate::minute(res_rgt_many$Date_time)} \cr
#' \code{RGT_cycle_14$second = lubridate::second(res_rgt_many$Date_time)} \cr
#' \code{RGT_cycle_14$RGT = res_rgt_many$RGT} \cr
#' \code{RGT_cycle_14} \cr
#'
#' @usage
#'
#' data(RGT_cycle_14)
#'
#' @keywords datasets
#'
#' @examples
#'
#' require(IceSat2R)
#' require(data.table)
#'
#' data(RGT_cycle_14)
"RGT_cycle_14"

