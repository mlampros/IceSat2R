
utils::globalVariables(c('..re_order_cols'))                        # used in the "verify_RGTs()" function


#' Get the data based on the API URL
#'
#' @param URL a character string specifying the API command
#' @param outputFormat a character string specifying the output format of the downloaded data. One of 'csv', 'json' or 'zip'
#' @param download_method a character string specifying the download method to use. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'. For more information see the documentation of the 'utils::download.file()' function
#' @param file_path_zip either NULL or a character string specifying a valid path to the output .zip file. This parameter will normally be a valid path if the 'outputFormat' parameter is set to 'zip'. If it's NULL and the 'outputFormat' parameter is 'zip' then the downloaded '.zip' file will be converted and returned as a data.table object
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return if the 'file_path_zip' parameter is NULL it returns either a data.table (if outputFormat is 'csv') or a nested list (if outputFormat is 'json') else the file path where the .zip file is saved. In case that the 'outputFormat' is set to 'zip' and the 'file_path_zip' parameter to NULL then a data.table will be returned.
#'
#' @importFrom glue glue
#' @importFrom data.table fread
#' @importFrom httr content GET
#' @importFrom utils download.file
#' @importFrom tools file_ext
#'
#' @keywords internal

get_URL_data = function(URL,
                        outputFormat = 'csv',
                        download_method = 'curl',
                        file_path_zip = NULL,
                        verbose = FALSE) {

  if (verbose) t_start = proc.time()

  query_response = httr::GET(url = URL)                         # this line is necessary even if I read directly with the data.table package to catch potential errors

  if (query_response$status_code != 200) {                      # code 200 satisfies the specified conditions
    openaltimetry_error_codes = list('400' = 'Bad request',
                                     '401' = 'Unauthorized',
                                     '500' = 'Internal error')

    url_error_code = openaltimetry_error_codes[[as.character(query_response$status_code)]]
    if (is.null(url_error_code)) {
      msg = glue::glue("The input URL: '{URL}'  returned error code '{query_response$status_code}'!")
    }
    else {
      msg = glue::glue("The input URL: '{URL}'  returned error code '{query_response$status_code}' ('{url_error_code}')!")
    }
    stop(msg, call. = F)
  }

  obj_out_flag = TRUE

  if (outputFormat == 'csv') {
    data_out = data.table::fread(URL, stringsAsFactors = F, header = T, verbose = F, showProgress = verbose)
  }
  else if (outputFormat == 'json') {
    data_out = httr::content(x = query_response, as = "parsed")
  }
  else if (outputFormat == 'zip') {
    if (is.null(file_path_zip)) {
      message("The 'file_path_zip' parameter is NULL therefore the downloaded .zip file will be converted to a data.table object!")
      file_path_zip = tempfile(fileext = '.zip')
    }
    else {
      obj_out_flag = FALSE
      ext_zip = tools::file_ext(file_path_zip)
      if (ext_zip != 'zip') stop("The extension of the 'file_path_zip' parameter must be '.zip'!", call. = F)
    }

    utils::download.file(url = URL, destfile = file_path_zip, method = download_method, quiet = !verbose)

    if (obj_out_flag) {
      data_out = data.table::fread(cmd = glue::glue('unzip -p {file_path_zip} *.csv'), stringsAsFactors = F, header = T, verbose = F, showProgress = verbose)
      if (file.exists(file_path_zip)) file.remove(file_path_zip)
    }
  }
  else {
    stop("Available output formats are 'csv', 'json' or 'zip'!", call. = F)
  }

  if (verbose) {
    cat('\n')
    compute_elapsed_time(time_start = t_start)
  }

  if (obj_out_flag) {
    return(data_out)
  }
  else {
    return(file_path_zip)
  }
}


#' Convert an abbreviated month name to a numeric value
#'
#' @param month a character string specifying one of the twelve months, i.e. ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#' @return a character string which will correspond to the numeric value of the input month, i.e. ("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#'
#' @keywords internal

switch_abb = function(month) {

  month = tolower(month)

  switch(month,
         jan = {mnth = '01'},
         feb = {mnth = '02'},
         mar = {mnth = '03'},
         apr = {mnth = '04'},
         may = {mnth = '05'},
         jun = {mnth = '06'},
         jul = {mnth = '07'},
         aug = {mnth = '08'},
         sep = {mnth = '09'},
         oct = {mnth = '10'},
         nov = {mnth = '11'},
         dec = {mnth = '12'},
  )
  return(mnth)
}


#' Convert a full month name to a numeric value
#'
#' @param month a character string specifying one of the twelve months, i.e. ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#' @return a character string which will correspond to the numeric value of the input month, i.e. ("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#'
#' @keywords internal

switch_full = function(month) {

  month = tolower(month)

  switch(month,
         january = {mnth = '01'},
         february = {mnth = '02'},
         march = {mnth = '03'},
         april = {mnth = '04'},
         may = {mnth = '05'},
         june = {mnth = '06'},
         july = {mnth = '07'},
         august = {mnth = '08'},
         september = {mnth = '09'},
         october = {mnth = '10'},
         november = {mnth = '11'},
         december = {mnth = '12'},
  )
  return(mnth)
}


#' Verification of the Reference Ground Tracks (RGTs)
#'
#' This function allows the user to verify the NSIDC extracted RGTs with the corresponding OpenAltimetry using the same Dates
#'
#' @param nsidc_rgts a data.frame, data.table or tibble object that includes the columns 'Date_time' and 'RGT'
#' @param bbx_aoi a named numeric vector or an sf-bbox object with names 'xmin', 'ymin', 'xmax', 'ymax'
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#' @param ... further parameters for the getTracks function
#'
#' @return a 'data.table' object where it is possible that the number of the OpenAltimetry RGTs is higher compared to the NSIDC RGTs
#'
#' @importFrom utils flush.console
#' @importFrom data.table setDT rbindlist
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' rgts = data.table::setDT(list(RGT = c(1251L, 1252L, 1260L, 1267L, 1275L),
#'                               Date_time = c("2020-12-15", "2020-12-15",
#'                               "2020-12-15", "2020-12-16", "2020-12-16")))
#' bbx = c(xmin = -53.108876, ymin = 60.119614, xmax = -19.203521, ymax = 80.793117)
#'
#' dtbl = verify_RGTs(nsidc_rgts = rgts, bbx_aoi = bbx, verbose = TRUE)
#' dtbl
#'
#' # split by Date to observe RGTs by date
#'
#' split(dtbl, by = 'Date_time')
#'
#' }

verify_RGTs = function(nsidc_rgts, bbx_aoi, verbose = FALSE, ...) {

  if (verbose) t_start = proc.time()
  if (!all(names(bbx_aoi) %in% c("xmin", "ymin", "xmax", "ymax"))) stop("The input parameter 'bbx_aoi' must be a named numeric vector or an sf-bbox object with names 'xmin', 'ymin', 'xmax', 'ymax'!", call. = F)

  tracks_dates_RGT = list()
  NROW = nrow(nsidc_rgts)

  for (item in 1:NROW) {

    if (verbose) {
      message("Row: ", item, "/", NROW, "\r", appendLF = FALSE)
      utils::flush.console()
    }

    dat_item = nsidc_rgts[item, , drop = F]
    Date = as.Date(dat_item$Date_time)
    char_date = as.character(Date)

    op_tra = getTracks(minx = as.numeric(bbx_aoi['xmin']),
                       miny = as.numeric(bbx_aoi['ymin']),
                       maxx = as.numeric(bbx_aoi['xmax']),
                       maxy = as.numeric(bbx_aoi['ymax']),
                       date = char_date,
                       verbose = FALSE, ...)

    if (nrow(op_tra) > 0) {
      colnames(op_tra) = 'RGT_OpenAlt'
      op_tra$Date_time = char_date
      op_tra$RGT_NSIDC = dat_item$RGT
    }
    else {
      op_tra = data.table::setDT(list(RGT_NSIDC = dat_item$RGT,
                                      RGT_OpenAlt = NA_integer_,
                                      Date_time = char_date))
    }

    tracks_dates_RGT[[char_date]] = op_tra
  }

  tracks_dates_RGT = data.table::rbindlist(tracks_dates_RGT)
  re_order_cols = c('Date_time', 'RGT_OpenAlt', 'RGT_NSIDC')
  tracks_dates_RGT = tracks_dates_RGT[, ..re_order_cols]
  if (verbose) compute_elapsed_time(time_start = t_start)

  return(tracks_dates_RGT)
}

