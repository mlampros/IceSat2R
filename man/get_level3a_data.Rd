% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_Atlas_data.R
\name{get_level3a_data}
\alias{get_level3a_data}
\title{Get IceSat-2 ATLAS 'Level-3A' data for a time interval (up to 1 year)}
\usage{
get_level3a_data(
  minx,
  miny,
  maxx,
  maxy,
  startDate,
  endDate,
  trackId,
  beamName = NULL,
  product = "atl08",
  client = "portal",
  outputFormat = "csv",
  file_path_zip = NULL,
  download_method = "curl",
  verbose = FALSE
)
}
\arguments{
\item{minx}{the 'minx' parameter of the bounding box}

\item{miny}{the 'miny' parameter of the bounding box}

\item{maxx}{the 'maxx' parameter of the bounding box}

\item{maxy}{the 'maxy' parameter of the bounding box}

\item{startDate}{a character string specifying the Data collection of the \emph{start} Date in the format 'yyyy-MM-dd' (such as '2020-01-01')}

\item{endDate}{a character string specifying the Data collection of the \emph{end} Date in the format 'yyyy-MM-dd' (such as '2020-01-01')}

\item{trackId}{an integer specifying the 'Reference ground track ID' (see the examples section on how to come to the 'trackId' and to a bounding box of 1 or 5 degrees globally)}

\item{beamName}{either NULL (data for all 6 beams will be returned) or a character vector specifying the Beam Name(s), It can be one or more of the 'gt1l', 'gt1r', 'gt2l', 'gt2r', 'gt3l' or 'gt3r'}

\item{product}{a character string specifying the input data product (default value is 'atl08'). It can be one of 'atl06', 'atl07', 'atl08', 'atl10', 'atl12' or 'atl13'}

\item{client}{a character string specifying the 'Referring client'. Can be one of 'portal' or 'jupyter' (default is 'portal')}

\item{outputFormat}{a character string specifying the output format of the downloaded data. One of 'csv', 'json' or 'zip'}

\item{file_path_zip}{either NULL or a character string specifying a valid path to the output .zip file. This parameter will normally be a valid path if the 'outputFormat' parameter is set to 'zip'. If it's NULL and the 'outputFormat' parameter is 'zip' then the downloaded '.zip' file will be converted and returned as a data.table object}

\item{download_method}{a character string specifying the download method to use. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'. For more information see the documentation of the 'utils::download.file()' function}

\item{verbose}{a boolean. If TRUE then information will be printed out in the console}
}
\value{
if the 'file_path_zip' parameter is NULL it returns either a data.table (if outputFormat is 'csv') or a nested list (if outputFormat is 'json') else the file path where the .zip file is saved. In case that the 'outputFormat' is set to 'zip' and the 'file_path_zip' parameter to NULL then a data.table will be returned.
}
\description{
This function allows the user to download IceSat-2 ATLAS 'Level-3A' data for a specific time interval, bounding box, track and beam.
}
\details{
Up to 1 year worth of ICESat-2 \emph{Level-3A} product data can be downloaded. \emph{Note:} requests are limited to 5x5 degree spatial bounding box selection

\describe{
 \item{'atl06', \emph{Land Ice Height} (Version 4)}{This data set \emph{(ATL06)} provides geolocated, land-ice surface heights (above the WGS 84 ellipsoid, ITRF2014 reference frame), plus ancillary parameters that can be used to interpret and assess the quality of the height estimates. The data were acquired by the Advanced Topographic Laser Altimeter System (ATLAS) instrument on board the Ice, Cloud and land Elevation Satellite-2 (ICESat-2) observatory}
 \item{'atl07', \emph{Sea Ice Height} (Version 4)}{The data set \emph{(ATL07)} contains along-track heights for sea ice and open water leads (at varying length scales) relative to the WGS84 ellipsoid (ITRF2014 reference frame) after adjustment for geoidal and tidal variations, and inverted barometer effects. Height statistics and apparent reflectance are also provided. The data were acquired by the Advanced Topographic Laser Altimeter System (ATLAS) instrument on board the Ice, Cloud and land Elevation Satellite-2 (ICESat-2) observatory}
 \item{'atl08', \emph{Land and Vegetation Height} (Version 4)}{This data set \emph{(ATL08)} contains along-track heights above the WGS84 ellipsoid (ITRF2014 reference frame) for the ground and canopy surfaces. The canopy and ground surfaces are processed in fixed 100 m data segments, which typically contain more than 100 signal photons. The data were acquired by the Advanced Topographic Laser Altimeter System (ATLAS) instrument on board the Ice, Cloud and land Elevation Satellite-2 (ICESat-2) observatory}
 \item{'atl10', \emph{Sea Ice Freeboard} (Version 4)}{This data set \emph{(ATL10)} contains estimates of sea ice freeboard, calculated using three different approaches. Sea ice leads used to establish the reference sea surface and descriptive statistics used in the height estimates are also provided. The data were acquired by the Advanced Topographic Laser Altimeter System (ATLAS) instrument on board the Ice, Cloud and land Elevation Satellite-2 (ICESat-2) observatory}
 \item{'atl12', \emph{Ocean Surface Height} (Version 4)}{This data set \emph{(ATL12)} contains along-track sea surface heights at variable length scales over cloud-free regions. Estimates of height distributions, surface roughness, surface slope, and apparent reflectance are also provided. The data were acquired by the Advanced Topographic Laser Altimeter System (ATLAS) instrument on board the Ice, Cloud and land Elevation Satellite-2 (ICESat-2) observatory}
 \item{'atl13', \emph{Inland Water Surface Height} (Version 4)}{This data set \emph{(ATL13)} contains along-track water surface heights and descriptive statistics for inland water bodies. Water bodies include lakes, reservoirs, bays, and estuaries. Descriptive statistics include along-track surface slope (where data permit), mean and standard deviation, subsurface signal (532 nm) attenuation, wave height, and coarse depth to bottom topography}
 }
}
\examples{

\dontrun{

require(IceSat2R)

#.......................................................
# parameter setting based on the output results from the
# 'examples' section of the 'get_atlas_data()' function
# so that we can iterate over the RGT's and Products for
# the specified time interval
#.......................................................

bbx = c(xmin = 140, ymin = -6.641235, xmax = 145, ymax = -1.641235)
start_date = "2021-02-03"
end_date = "2021-02-15"
RGTs = c(627, 756, 688, 619, 817)
Products = c('atl08', 'atl13')

#........................................................
# loop over the specified parameters and save the results
#........................................................

dat_out = logs_out = list()

for (prod_i in Products) {
  for (track_i in RGTs) {

    name_iter = glue::glue("{track_i}_{prod_i}")
    cat(glue::glue("RGT: '{track_i}'  Product: '{prod_i}'"), '\n')

    iter_dat = get_level3a_data(minx = as.numeric(bbx['xmin']),
                                miny = as.numeric(bbx['ymin']),
                                maxx = as.numeric(bbx['xmax']),
                                maxy = as.numeric(bbx['ymax']),
                                startDate = start_date,
                                endDate = end_date,
                                trackId = track_i,
                                beamName = NULL,        # return data of all 6 beams
                                product = prod_i,
                                client = 'portal',
                                outputFormat = 'csv',
                                verbose = FALSE)

    iter_logs = list(RGT = track_i,
                     Product = prod_i,
                     N_rows = nrow(iter_dat))

    logs_out[[name_iter]] = data.table::setDT(iter_logs)
    dat_out[[name_iter]] = iter_dat
  }
}


#.........................................
# each sublist corresponds to a different
# parameter setting (Track, Product)
#.........................................

dat_out

#.....
# Logs  (including the number of rows for each parameter setting)
#.....

dtbl_logs = data.table::rbindlist(logs_out)
dtbl_logs = subset(dtbl_logs, N_rows > 0)
dtbl_logs = dtbl_logs[order(dtbl_logs$N_rows, decreasing = T), ]
dtbl_logs

}
}
\references{
https://openaltimetry.earthdatacloud.nasa.gov

https://nsidc.org/data/icesat-2
}
