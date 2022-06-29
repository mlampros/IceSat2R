
utils::globalVariables(c('i',                        # for the foreach for-loop
                         'Type'))                    # for the Type column when I use the base R subset() function


#' Extraction of the url from the Technical Specification Website
#'
#' This function allows the user to view the latest 'Nominal' and 'Time Specfic' orbit metadata (Url, Reference Ground Track Names, Dates and Types)
#'
#' @param technical_specs_url a character string specifying the technical specs website
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return a 'data.table' object
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @importFrom rvest read_html html_elements
#' @importFrom data.table data.table
#' @import magrittr
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' orbs = latest_orbits(verbose = TRUE)
#' orbs
#'
#' }

latest_orbits = function(technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
                         verbose = FALSE) {
  if (verbose) {
    t_start = proc.time()
    cat("Access the data of the technical specs website ...\n")
  }

  pg = rvest::read_html(technical_specs_url)

  if (verbose) cat("Extract the .zip files and the corresponding titles ...\n")
  url_zipfiles = pg %>%
    rvest::html_elements("a[href$='.zip']") %>%       # see: https://stackoverflow.com/a/48146333
    as.character()
  # rvest::html_text2()                               # returns only the url-titles

  if (verbose) cat("Keep the relevant data from the url's and titles ...\n")
  df_out = lapply(seq_along(url_zipfiles), function(x) {
    item = url_zipfiles[x]
    item = gsub(pattern = '<a href=\"', replacement = '', x = item)
    item = gsub(pattern = '</a>', replacement = '', x = item)

    idx_str = which(gregexpr(pattern = '<strong>', text = item) != -1)
    if (length(idx_str) > 0) {
      item = gsub(pattern = '<strong>', replacement = '', x = item)
      item = gsub(pattern = '</strong>', replacement = '', x = item)
    }

    item = strsplit(x = item, split = '[\">]')[[1]]
    item = item[item != ""]
    item = trimws(x = item, which = 'both')
    item
  })

  df_out = do.call(rbind, df_out)

  if (verbose) cat("Process the nominal and time specific orbits separately ...\n")
  idx_time_spec = which(gregexpr(pattern = "^RGTs", text = df_out[, 2]) != -1)
  nominal_orbits = df_out[-idx_time_spec, ]
  time_specific = df_out[idx_time_spec, ]

  if (verbose) cat("Adjust the Dates of the time specific orbits ...\n")
  dates_spec = as.vector(unlist(lapply(strsplit(x = time_specific[, 2], split = "for"), function(x) trimws(x[2], which = 'both'))))
  dates_spec = strsplit(x = dates_spec, split = '-')
  dates_spec = lapply(dates_spec, function(x) {
    x = trimws(x = x, which = 'both')
    x = gsub(pattern = "[(]", replacement = ",", x = x)
    x = gsub(pattern = "[)]", replacement = "", x = x)
    x = strsplit(x = x, split = ',')

    x_out = rep(x = NA_character_, 2)

    for (i in seq_along(x)) {
      y = trimws(x = x[[i]], which = 'both')
      if (length(y) == 1) {
        y = c(y, trimws(x = x[[i+1]][2], which = 'both'))
      }
      x_out[i] = paste(y, collapse = ' ')
    }
    x_out
  })

  #.................................................. conversion to a date requires to take into account system locale (see: https://github.com/mlampros/IceSat2R/issues/3)
  # dates_spec = lapply(dates_spec, function(x) {
  #   as.character(as.Date(x, format = "%b %d %Y"))
  # })
  #.................................................. therefore convert from different date format using simple character string processing

  dates_spec = lapply(dates_spec, function(x) {
    as.vector(sapply(x, function(y) {
      as.vector(unlist(lapply(strsplit(y, ' '), function(z) {
        day_item = ifelse(nchar(z[2]) == 1, paste0('0', z[2]), z[2])
        as.character(glue::glue("{z[3]}-{switch_full(z[1])}-{day_item}"))
      })))
    }))
  })

  any_nas = as.vector(unlist(lapply(dates_spec, function(x) any(is.na(x)))))
  if (any(any_nas)) stop("The conversion to dates returned missing values!", call. = F)

  dates_spec = do.call(rbind, dates_spec)

  if (verbose) cat("Create the nominal orbits data.table ...\n")
  nominal_orbits = data.table::data.table(nominal_orbits, stringsAsFactors = F)
  colnames(nominal_orbits) = c('Url', 'RGT')
  nominal_orbits$Date_from = NA_character_
  nominal_orbits$Date_to = NA_character_
  nominal_orbits$Type = 'Nominal'

  if (verbose) cat("Create the time specific orbits data.table ...\n")
  time_specific = data.table::data.table(time_specific[, 1, drop = F], stringsAsFactors = F)
  colnames(time_specific) = 'Url'
  time_specific$RGT = trimws(gsub(pattern = '.zip', '', basename(path = time_specific$Url)), which = 'both')
  time_specific$Date_from = dates_spec[, 1]
  time_specific$Date_to = dates_spec[, 2]
  time_specific$Type = 'Time_Specific'

  if (verbose) cat("Return a single data.table ...\n")
  res_both = rbind(nominal_orbits, time_specific)
  if (verbose) compute_elapsed_time(time_start = t_start)

  return(res_both)
}



#' Nominal mission orbits
#'
#' This function allows the user to view the nominal orbits (all or a selection)
#'
#' @param orbit_area either NULL or a character string specifying the earth partition to use, it can be one of 'antarctic', 'arctic', 'western_hemisphere' and 'eastern_hemisphere'
#' @param technical_specs_url a character string specifying the technical specs website
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return a list object with the available nominal orbits
#'
#' @importFrom glue glue
#' @importFrom data.table fread
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @export
#'
#' @examples
#'
#' require(IceSat2R)
#'
#' #.............................
#' # all available nominal orbits
#' #.............................
#'
#' nomin_orb = available_nominal_orbits(verbose = TRUE)
#' nomin_orb
#'
#' #........................
#' # specific nominal orbits
#' #........................
#'
#' nomin_orb = available_nominal_orbits(orbit_area = 'arctic',
#'                                      verbose = TRUE)
#' nomin_orb
#'

available_nominal_orbits = function(orbit_area = NULL,
                                    technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
                                    verbose = FALSE) {

  if (verbose) cat(glue::glue("The available Icesat-2 orbits will be red from '{technical_specs_url}' ..."), '\n')
  avail_urls = tryCatch(latest_orbits(technical_specs_url = technical_specs_url, verbose = verbose), error = function(e) e)
  if (inherits(avail_urls, "error")) {
    if (verbose) message(glue::glue("The 'latest_orbits()' function gave the following error: '{avail_urls$message}'! The existing orbits file will be uploaded!"))
    avail_urls = data.table::fread(file = system.file('data_files', 'technical_specs_urls.csv', package = "IceSat2R"), stringsAsFactors = F, header = T)
  }

  nom_orb = subset(avail_urls, Type == 'Nominal')

  rgts = as.vector(unlist(lapply(strsplit(x = nom_orb$RGT, split = ' '), function(x) {
    if (length(x) == 2) {
      x = x[1]
    }
    else {
      x = x[1:2]
    }
    x = paste(x, collapse = '_')
    tolower(x)
  })))

  nom_orb$RGT = rgts
  intern_orbits = list()
  for (i in 1:nrow(nom_orb)) {
    intern_orbits[[nom_orb$RGT[i]]] = nom_orb$Url[i]
  }

  if (!is.null(orbit_area)) {
    if (!all(orbit_area %in% names(intern_orbits))) stop(glue::glue("The 'orbit_area' parameter can be one of {paste(rgts, collapse = ', ')}!"), call. = F)
    intern_orbits = intern_orbits[orbit_area]
  }

  return(intern_orbits)
}



#' Overall Mission Orbits
#'
#' This function allows the user to view information of the nominal mission orbits and beam locations: "The processed files have 7 tracks per
#' orbit: one for each of the six beams of ICESat-2, and the seventh for the Reference Ground Track (RGT). The RGT is an imaginary line
#' through the six-beam pattern that is handy for getting a sense of where the orbits fall on Earth, and which the mission uses to point the
#' observatory. However, the six tracks for the six beams are our best estimate of where the beams will actually fall on Earth's surface."
#'
#' @param orbit_area a character string specifying the nominal mission orbits and beam locations. It can be one of 'antarctic', 'arctic', 'western_hemisphere' or 'eastern_hemisphere'
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param threads an integer that specifies the number of threads to use in parallel when processing the data
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an 'sf' object of multiple tracks (see the 'LAYER' column of the output object)
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom tools file_ext
#' @importFrom doParallel registerDoParallel
#' @importFrom sf st_layers st_read st_as_sf st_zm
#' @importFrom data.table rbindlist
#' @import foreach
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' res_orb  = overall_mission_orbits(orbit_area = 'antarctic',
#'                                   download_method = 'curl',
#'                                   threads = 1,
#'                                   verbose = TRUE)
#' str(res_orb)
#' }

overall_mission_orbits = function(orbit_area,
                                  download_method = 'curl',
                                  threads = 1,
                                  verbose = FALSE) {
  if (verbose) {
    t_start = t_downl = proc.time()
  }

  orbits = available_nominal_orbits(orbit_area = orbit_area, verbose = verbose)

  tmp_orbit_file = tempfile(fileext = '.zip')
  if (verbose) cat(glue::glue("The orbits of '{orbit_area}' will be downloaded (temporarily) in the '{tmp_orbit_file}' file ..."), '\n')
  utils::download.file(url = orbits[[orbit_area]], destfile = tmp_orbit_file, method = download_method, quiet = !verbose)
  tmp_orbits_dir = file.path(tempdir(), glue::glue('orbits_{orbit_area}'))                                # create a second directory inside the temporary directory to extract the .zip file
  if (!dir.exists(tmp_orbits_dir)) dir.create(tmp_orbits_dir)

  if (verbose) cat(glue::glue("The downloaded .zip file will be extracted in the '{tmp_orbits_dir}' directory ..."), '\n')
  utils::unzip(zipfile = tmp_orbit_file, exdir = tmp_orbits_dir, junkpaths = T)

  lst_kmz = list.files(path = tmp_orbits_dir, full.names = F, pattern = '.kmz')
  LEN = length(lst_kmz)
  if (LEN == 0) stop(glue::glue("The directory '{tmp_orbits_dir}' does not include any .kmz files!"), call. = F)
  EXT = unique(tools::file_ext(lst_kmz))[1]

  if (verbose) {
    cat(glue::glue("Download and unzip the {LEN} '.{EXT}' files: "))
    compute_elapsed_time(time_start = t_downl)
    t_proc = proc.time()
    cat(glue::glue("An 'sf' object will be created from the {LEN} .kmz files."), '\n')
  }

  if (threads > 1 & LEN > 1) {
    if (verbose) cat(glue::glue("Parallel processing of {LEN} '.{EXT}' files using  {threads}  threads starts ..."), '\n')

    if (.Platform$OS.type == "unix") {
      doParallel::registerDoParallel(cores = threads)
    }

    if (.Platform$OS.type == "windows") {
      cl = parallel::makePSOCKcluster(threads)
      doParallel::registerDoParallel(cl = cl)               # compared to unix, ".. if not specified, on Windows a three worker cluster is created and used .." [ see also: https://stackoverflow.com/a/45122448/8302386 ]
    }

    res_out = foreach::foreach(i = 1:LEN) %dopar% {         # parallelize the 'OUTER' loop because I have few layers
      FILE = lst_kmz[i]
      iter_file = file.path(tmp_orbits_dir, FILE)
      lrs = sf::st_layers(dsn = iter_file)                  # fist get the layers
      NAMS = lrs$name
      if (length(NAMS) == 0) stop(glue::glue("The file '{FILE}' does not include any layers!"), call. = F)
      nam_file = gsub('.kmz', '', FILE)
      sf_objs = list()

      for (LAYER in NAMS) {
        lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)
        nam_layer = gsub('.kml', '', LAYER)
        lr_dat$LAYER = rep(nam_layer, nrow = lr_dat)
        sf_objs[[glue::glue("{nam_file}_{nam_layer}")]] = lr_dat
      }

      sf_objs
    }

    res_out = unlist(res_out, recursive = F)             # in case of nested lists return a single list object removing sublists
  }
  else {

    if (verbose) cat(glue::glue("A single thread will be used to process the '.{EXT}' files ..."), '\n')
    res_out = list()

    for (FILE in lst_kmz) {
      iter_file = file.path(tmp_orbits_dir, FILE)
      lrs = sf::st_layers(dsn = iter_file)
      NAMS = lrs$name
      if (length(NAMS) == 0) stop(glue::glue("The file '{FILE}' does not include any layers!"), call. = F)

      nam_file = gsub('.kmz', '', FILE)

      for (LAYER in NAMS) {
        if (verbose) cat(glue::glue("File '{FILE}' and Layer '{LAYER}' will be processed ..."), '\n')
        lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)
        nam_layer = gsub('.kml', '', LAYER)
        lr_dat$LAYER = rep(nam_layer, nrow = lr_dat)
        res_out[[glue::glue("{nam_file}_{nam_layer}")]] = lr_dat
      }
    }
  }

  if (verbose) {
    cat(glue::glue("Processing of the {LEN} '.{EXT}' files: "))
    compute_elapsed_time(time_start = t_proc)
  }

  if (verbose) cat("The temproary files will be removed ...\n")
  for (tmp_file in lst_kmz) {                                          # remove the .kmz files
    pth_kmz = file.path(tmp_orbits_dir, tmp_file)
    if (file.exists(pth_kmz)) file.remove(pth_kmz)
  }
  if (file.exists(tmp_orbit_file)) file.remove(tmp_orbit_file)         # remove the .zip file

  if (verbose) cat(glue::glue("Create a single 'sf' object from the {length(res_out)} sublists and drop the Z dimension ..."), '\n')
  res_out = sf::st_as_sf(data.table::rbindlist(res_out))
  res_out = sf::st_zm(x = res_out, drop = T)                                                 # drop the Z dimension

  if (verbose) {
    cat("Total ")
    compute_elapsed_time(time_start = t_start)
  }
  return(res_out)
}



#' Reference Ground Tracks (RGTs)
#'
#' This function returns the url's of all Reference Ground Track (RGT) cycles. Moreover, it returns the dates that belong to each RGT cycle and the names of the RGT cycles.
#'
#' @param only_cycle_names a boolean. If TRUE then only the RGT (Reference Ground Track) cycle names will be returned. Otherwise all orbit files, dates and cycle names.
#' @param technical_specs_url a character string specifying the technical specs website
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return a list object with the available orbit files, dates and cycle names
#'
#' @importFrom glue glue
#' @importFrom data.table fread
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' #.................................................
#' # all available orbit files, dates and cycle names
#' #.................................................
#'
#' avail_dat = available_RGTs(only_cycle_names = FALSE,
#'                            verbose = TRUE)
#' avail_dat
#'
#' #.............................
#' # receive only the cycle names
#' #.............................
#'
#' avail_cycles = available_RGTs(only_cycle_names = TRUE,
#'                               verbose = TRUE)
#' avail_cycles
#'
#' }


available_RGTs = function(only_cycle_names = FALSE,
                          technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs",
                          verbose = FALSE) {

  if (verbose) cat(glue::glue("The available Icesat-2 orbits will be red from '{technical_specs_url}' ..."), '\n')
  avail_urls = tryCatch(latest_orbits(technical_specs_url = technical_specs_url, verbose = verbose), error = function(e) e)
  if (inherits(avail_urls, "error")) {
    if (verbose) message(glue::glue("The 'latest_orbits()' function gave the following error: '{avail_urls$message}'! The existing orbits file will be uploaded!"))
    avail_urls = data.table::fread(file = system.file('data_files', 'technical_specs_urls.csv', package = "IceSat2R"), stringsAsFactors = F, header = T)
  }

  time_spec = subset(avail_urls, Type == 'Time_Specific')
  time_spec$Date_from = as.Date(time_spec$Date_from)
  time_spec$Date_to = as.Date(time_spec$Date_to)
  time_spec = time_spec[order(time_spec$Date_from, decreasing = F), ]              # sort by date to have the RGT-cycles ordered in increasing order

  orbit_files = orbit_dates = list()

  for (i in 1:nrow(time_spec)) {
    nam_i = glue::glue("RGT_cycle_{i}")
    orbit_files[[nam_i]] = time_spec$Url[i]
    orbit_dates[[nam_i]] = seq(from = time_spec$Date_from[i], to = time_spec$Date_to[i], by = '1 day')
  }

  orb_nams = names(orbit_files)

  if (only_cycle_names) {
    return(orb_nams)
  }
  else {
    return(list(orbit_files = orbit_files,
                orbit_dates = orbit_dates,
                names_cycles = orb_nams))
  }
}



#' Revisit Time Reference Ground Tracks and Dates
#'
#' This function shows the information of the 'available_RGTs' function and additionally it returns the .zip (kmz files) of all laser
#' tracks over each 91-day repeat period (revisit time). Note that the locations and times are estimates, but should be correct to
#' within a few minutes in time and better than 100m in the predicted locations.
#'
#' @param RGT_cycle either NULL or a character string specifying a single RGT (Reference Ground Track) as determined by the output of the 'available_RGTs(only_cycle_names = TRUE)' function. If NULL then all available Data will be returned.
#' @param complete_date_sequence a boolean. If TRUE a complete sequence of Dates will be returned, otherwise only the 'minimum' and 'maximum' Dates.
#'
#' @return a list object with the available orbit files, dates and date sequence lengths
#'
#' @details
#'
#' ICESat-2 was in safe-hold from June 26 through July 9, 2019. ATLAS was off during this time, so data was not collected or pointed to the reference ground track.
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @importFrom glue glue
#'
#' @export
#'
#' @examples
#'
#' require(IceSat2R)
#'
#' #.......................................................
#' # receive all orbit files, dates and length of sequences
#' #.......................................................
#'
#' rev_all = revisit_time_RGTs(RGT_cycle = NULL, complete_date_sequence = TRUE)
#' rev_all
#'
#' #...................................................
#' # observe and choose one of the available RGT-cycles
#' #...................................................
#'
#' avail_cycles = available_RGTs(only_cycle_names = TRUE,
#'                               verbose = TRUE)
#' avail_cycles
#'
#' #.....................................
#' # receive results for a specific cycle
#' #.....................................
#'
#' rev_cycle = revisit_time_RGTs(RGT_cycle = 'RGT_cycle_1', complete_date_sequence = FALSE)
#' rev_cycle


revisit_time_RGTs = function(RGT_cycle = NULL,
                             complete_date_sequence = FALSE) {

  avail_rgts = available_RGTs(only_cycle_names = FALSE)
  orbit_files = avail_rgts$orbit_files
  orbit_dates = avail_rgts$orbit_dates
  orb_nams = names(orbit_files)

  if (!is.null(RGT_cycle)) {

    if (!RGT_cycle %in% orb_nams) {
      quote_nams = as.vector(sapply(orb_nams, function(x) glue::glue("'{x}'")))
      concat_nams = paste(quote_nams, collapse = ', ')
      stop(glue::glue("Available cycles are the following: {concat_nams}"), call. = F)
    }

    orbit_files = orbit_files[[RGT_cycle]]
    orbit_dates = orbit_dates[[RGT_cycle]]
    LEN_seq = length(orbit_dates)

    if (!complete_date_sequence) {
      orbit_dates = c(min(orbit_dates), max(orbit_dates))
    }
  }
  else {
    LEN_seq = lapply(orbit_dates, function(x) length(x))

    if (!complete_date_sequence) {
      orbit_dates = lapply(orbit_dates, function(x) {
        c(min(x), max(x))
      })
    }
  }

  return(list(orbit_files = orbit_files,
              orbit_dates = orbit_dates,
              length_dates = LEN_seq))
}



#' Time Specific Orbits
#'
#' This function shows the reference ground track time and locations for specific date ranges. "Updated KML files have been posted to the 'tech-specs'
#' website (see the 'references' section for more details) containing individual files for each Reference Ground Track (RGT) with a date and time stamp
#' posted every 420 kilometers along-track (roughly 1 minute of flight time in between each point). The first RGT is 234; this is where the time series
#' begins. The date of each RGT is in the file name, so the user can easily ascertain where and when ICESat-2 will be on a particular day."
#'
#' @param date_from either NULL or a character string specifying the start date in the format 'yyyy-MM-dd' (such as '2020-01-01'). If this parameter is NULL then the 'RGT_cycle' parameter must be specified
#' @param date_to either NULL or a character string specifying the end date in the format 'yyyy-MM-dd' (such as '2020-01-01'). If this parameter is NULL then the 'RGT_cycle' parameter must be specified
#' @param RGT_cycle a character vector specifying the RGT (Reference Ground Track) cycle(s) (the specific revisit times of ICESAT-2). This parameter can be greater or equal to 1 with a maximum 'RGT-cycle' names as determined by the output of the 'available_RGTs(only_cycle_names = TRUE)' function. The computation time of a single 'RGT-cycle' might take approximately 15 minutes utilizing 8 threads (in parallel) and require approximately 2 GB of memory.
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param threads an integer that specifies the number of threads to use in parallel when processing the data
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an 'sf' object that will include one or more Reference Ground Tracks  (see the 'RGT' column of the output object)
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip flush.console
#' @importFrom doParallel registerDoParallel
#' @importFrom sf st_layers st_read st_as_sf st_zm
#' @importFrom data.table setDT rbindlist data.table
#' @import foreach
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' #................................................
#' # RGTs (Reference Ground Tracks) for a single day
#' #................................................
#'
#' res_rgt_one = time_specific_orbits(date_from = '2019-06-01',
#'                                    date_to = '2019-06-01',
#'                                    download_method = 'curl',
#'                                    threads = 1,
#'                                    verbose = TRUE)
#' str(res_rgt_one)
#'
#' #..........................................................
#' # RGTs (Reference Ground Tracks) for a specific time period
#' #..........................................................
#'
#' res_rgt_many = time_specific_orbits(date_from = '2019-06-01',
#'                                     date_to = '2019-06-03',
#'                                     download_method = 'curl',
#'                                     threads = 1,
#'                                     verbose = TRUE)
#' str(res_rgt_many)
#'
#'
#' #.........................................................
#' # processing more than one RGTs for a specified date range
#' #.........................................................
#'
#' res_rgt_inters = time_specific_orbits(date_from = '2021-03-23',
#'                                       date_to = '2021-03-26',
#'                                       download_method = 'curl',
#'                                       threads = 1,
#'                                       verbose = TRUE)
#' str(res_rgt_inters)
#'
#' table(res_rgt_inters$cycle)
#' table(res_rgt_inters$day_of_year)
#' table(res_rgt_inters$RGT)
#'
#' #...............................................................
#' # RGTs (Reference Ground Tracks) for a selected 'cycle'
#' # Observe the available RGT-cycles and use all available threads
#' #...............................................................
#'
#' avail_cycles = available_RGTs(only_cycle_names = TRUE,
#'                               verbose = TRUE)
#' avail_cycles
#'
#' choose_cycle = avail_cycles[3]
#'
#' res_rgt_many = time_specific_orbits(RGT_cycle = choose_cycle,
#'                                     download_method = 'curl',
#'                                     threads = parallel::detectCores(),
#'                                     verbose = TRUE)
#' }

time_specific_orbits = function(date_from = NULL,
                                date_to = NULL,
                                RGT_cycle = NULL,
                                download_method = 'curl',
                                threads = 1,
                                verbose = FALSE) {

  if (verbose) t_start = proc.time()
  if (all(c(!is.null(date_from), !is.null(date_to), !is.null(RGT_cycle)))) stop("You have to specify either an RGT cycle ('RGT_cycle' parameter) or the 'date_from' and 'date_to' parameters!", call. = F)

  avail_rgts = available_RGTs(only_cycle_names = FALSE, verbose = verbose)
  orbit_files = avail_rgts$orbit_files
  orbit_dates = avail_rgts$orbit_dates
  cycle_names = avail_rgts$names_cycles

  rgt_cycle_null = is.null(RGT_cycle)

  if (rgt_cycle_null) {

    if (any(c(is.null(date_from), is.null(date_to)))) {
      stop("If the 'RGT_cycle' parameter is NULL then both 'date_from' and 'date_to' must be specified!", call. = F)
    }
    else {

      orbit_dates = lapply(seq_along(cycle_names), function(x) {
        iter_lst = list(date = orbit_dates[[x]])
        iter_dates = data.table::setDT(iter_lst)
        iter_dates$cycle = rep(cycle_names[x], nrow(iter_dates))
        iter_dates
      })

      orbit_dates = data.table::rbindlist(orbit_dates)
      orbit_dates = orbit_dates[order(orbit_dates$date, decreasing = F), ]

      complete_dates = seq(from = orbit_dates$date[1], to = orbit_dates$date[length(orbit_dates$date)], by = '1 day')
      complete_dates = data.table::setDT(list(date = complete_dates))
      orbit_dates = merge(orbit_dates, complete_dates, by = 'date', all = T)
      orbit_dates = orbit_dates[order(orbit_dates$date, decreasing = F), ]
      if (verbose) cat(glue::glue("ICESAT-2 orbits: 'Earliest-Date' is '{orbit_dates$date[1]}'  'Latest-Date' is '{orbit_dates$date[length(orbit_dates$date)]}'"), '\n')

      if (!inherits(date_from, 'Date')) date_from = as.Date(date_from)
      if (!inherits(date_to, 'Date')) date_to = as.Date(date_to)
      if (date_from > date_to) stop("The 'date_from' parameter must not be greater than the 'date_to' parameter!", call. = F)

      idx_select = which(orbit_dates$date >= date_from & orbit_dates$date <= date_to)
      if (length(idx_select) > 0) {
        orbit_dates = orbit_dates[idx_select, , drop = F]

        idx_missing = which(is.na(orbit_dates$cycle))
        if (length(idx_missing) > 0) {
          message(glue::glue("There are {length(idx_missing)} missing values (NA's) in the selected Date range from '{date_from}' to '{date_to}' and will be removed (for more information consult the 'https://icesat-2.gsfc.nasa.gov/science/specs' webpage)!"))
          orbit_dates = orbit_dates[-idx_missing, , drop = F]
          if (nrow(orbit_dates) == 0) {
            stop("After removing the missing values an empty data.table was returned! Please consult the 'https://icesat-2.gsfc.nasa.gov/science/specs' webpage for more information!", call. = F)
          }
        }
      }
      else {
        stop("There are no intersected Dates between the ICESAT-2 orbit files and the input 'date_from' and 'date_to' parameters!", call. = F)
      }

      RGT_cycle = unique(orbit_dates$cycle)
    }
  }

  all_cycles = list()

  for (CYCLE in RGT_cycle) {

    if (verbose) {
      cat('-----------------------------------------------------\n')
      cat(glue::glue("The .zip file of '{CYCLE}' will be downloaded ..."), '\n')
      cat('-----------------------------------------------------\n')
      t_downl = proc.time()
    }

    URL_zip = orbit_files[[CYCLE]]

    tmp_orbit_file = tempfile(fileext = '.zip')
    utils::download.file(url = URL_zip, destfile = tmp_orbit_file, method = download_method, quiet = !verbose)
    tmp_orbits_dir = file.path(tempdir(), CYCLE)                                                  # create a second directory inside the temporary directory to extract the .zip file
    if (!dir.exists(tmp_orbits_dir)) dir.create(tmp_orbits_dir)

    if (verbose) cat(glue::glue("The downloaded .zip file will be extracted in the '{tmp_orbits_dir}' directory ..."), '\n')
    utils::unzip(zipfile = tmp_orbit_file, exdir = tmp_orbits_dir, junkpaths = T)

    if (verbose) {
      cat(glue::glue("Download and unzip the {CYCLE} .zip file: "))
      compute_elapsed_time(time_start = t_downl)
    }

    lst_kmz = list.files(path = tmp_orbits_dir, full.names = F, pattern = '.kml')                                                     # there are 2 kind of files in the directory that begin with 'IS2_RGT_...' and '._IS2_RGT_..' [ the last type beginning with dot '.' is not a regular .kml file and won't be processed ]
    LEN = length(lst_kmz)
    if (LEN == 0) stop(glue::glue("The directory '{tmp_orbits_dir}' does not include any .kmz files!"), call. = F)

    dtbl_files = data.table::setDT(list(files = lst_kmz))
    vec_dates_files = trimws(as.vector(unlist(lapply(strsplit(lst_kmz, "[_.]"), function(x) x[length(x) - 1]))), which = 'both')
    # vec_dates_files = as.Date(strptime(x = vec_dates_files, format = "%d-%b-%Y"))                                                   # this command might not work for specific system locale
    vec_dates_files = as.vector(unlist(lapply(strsplit(vec_dates_files, '-'), function(x) {
      iter_month = switch_abb(x[2])
      as.character(glue::glue("{x[3]}-{iter_month}-{x[1]}"))
    })))
    vec_dates_files = as.Date(vec_dates_files)
    dtbl_files$date = vec_dates_files

    if (rgt_cycle_null) {

      merg_dat = merge(orbit_dates, dtbl_files, by = 'date')

      if (nrow(merg_dat) == 0) {          # make sure that after merging I have files for processing
        print(orbit_dates)
        print("------------------")
        print(dtbl_files)
        stop("There is no intersection between the orbit file dates and the downloaded .zip file .kml files!", call. = F)
      }
    }
    else {
      merg_dat = dtbl_files
    }

    LEN_subset = nrow(merg_dat)
    if (verbose) {
      cat(glue::glue("{LEN_subset} .kml files will be processed ..."), '\n')
      t_proc = proc.time()
    }

    sf_objs = list()

    if (threads > 1 & LEN_subset > 1) {
      if (verbose) cat(glue::glue("Parallel processing of {LEN_subset} .kml files using  {threads}  threads starts ..."), '\n')

      if (.Platform$OS.type == "unix") {
        doParallel::registerDoParallel(cores = threads)
      }

      if (.Platform$OS.type == "windows") {
        cl = parallel::makePSOCKcluster(threads)
        doParallel::registerDoParallel(cl = cl)               # compared to unix, ".. if not specified, on Windows a three worker cluster is created and used .." [ see also: https://stackoverflow.com/a/45122448/8302386 ]
      }
    }
    else {
      if (verbose) cat(glue::glue("Sequential processing of {LEN_subset} .kml files using a single thread starts ..."), '\n')
    }

    for (idx in 1:LEN_subset) {
      if (verbose) {
        message("FILE: ", idx, "/", LEN_subset, "\r", appendLF = FALSE)
        utils::flush.console()
      }

      FILE = merg_dat$files[idx]
      iter_file = file.path(tmp_orbits_dir, FILE)
      lrs = sf::st_layers(dsn = iter_file)
      NAMS = lrs$name
      if (length(NAMS) == 0) stop(glue::glue("The file '{FILE}' does not include any layers!"), call. = F)
      nam_file = gsub('.kml', '', FILE)
      LEN_nams = length(NAMS)

      if (threads > 1 & LEN_subset > 1) {
        inner_obj = foreach::foreach(i = 1:LEN_nams) %dopar% {
          LAYER = NAMS[i]
          lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)         # I expect each .kml file to consist of a single 'sfc_POINT'
          lr_dat
        }
      }
      else {
        inner_obj = lapply(1:LEN_nams, function(x) {
          LAYER = NAMS[x]
          lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)         # I expect each .kml file to consist of a single 'sfc_POINT'
          lr_dat
        })
      }

      # class_obj = as.vector(unlist(lapply(inner_obj, function(x) class(sf::st_geometry(x))[1])))                      # I expect each sublist to be of type "sfc_POINT" and normally observations which are not (and can be for instance "sfc_LINESTRING") won't have a description column either. Therefore use the next line for removal
      descr_not_idx = which(as.vector(unlist(lapply(inner_obj, function(x) {
        colnams = colnames(x)
        if ('description' %in% colnams) {
          verify_descr = (is.na(x$description) | x$description == "")
        }
        else if ('Description' %in% colnams) {
          verify_descr = (is.na(x$Description) | x$Description == "")      # In Macintosh (osx) the "description" column appears with an upper case "D" as "Description", see the following issue: https://github.com/mlampros/IceSat2R/issues/9#issuecomment-1152020607
        }
        else {
          stop("The 'D(d)escription' column must exist in every sublist!", call. = F)
        }
        verify_descr
      }))))
      
      LEN_exc = length(descr_not_idx)
      if (LEN_exc > 0) {
        if (verbose) message(glue::glue("{LEN_exc} output sublist did not have a valid 'description' and will be removed!"))
        inner_obj = inner_obj[-descr_not_idx]
      }

      inner_obj = sf::st_as_sf(data.table::rbindlist(inner_obj))
      sf_objs[[glue::glue("{nam_file}")]] = inner_obj
    }

    sf_objs = sf::st_as_sf(data.table::rbindlist(sf_objs))

    if (verbose) cat("The 'description' column of the output data will be processed ...\n")
    # descr_proc = strsplit(x = sf_objs$description, split = '[ \n]')                           # split either by newline or by space  [ it's wrong because it splits the date and time too ]
    
    colnams_dtbl = colnames(sf_objs)
    flag_upper_descr = FALSE
    if ('description' %in% colnams_dtbl) {                                 # see previous exception regarding "D(d)escription" (here I assume that only "description" or "Description" cases exist)
      sf_objs$description = as.character(sf_objs$description)
      descr_proc = strsplit(x = sf_objs$description, split = '\n')
    }
    else {
      sf_objs$Description = as.character(sf_objs$Description)
      descr_proc = strsplit(x = sf_objs$Description, split = ' ')          # in case that I have a 'Description' column, split by empty space
      chk_row_items = as.vector(unlist(lapply(descr_proc, function(x) (length(x) %% 2) == 0)))     # check that I have an even number of columns (normally 8 but can be fewer too) then concatenate the columns by pairs of consecutive items
      if (!all(chk_row_items)) stop("It seems that after splitting the observations by empty space the number of columns (per row) are not an even number!", call. = F)
      descr_proc = lapply(descr_proc, function(x) {
        seq_item = seq(from = 1, to = length(x), by = 2)
        sapply(seq_item, function(y) paste(c(x[y], x[y+1]), collapse = ' '))
      })
      flag_upper_descr = TRUE
    }
    descr_proc = data.table::data.table(do.call(rbind, descr_proc), stringsAsFactors = F)

    #............................................................................................................................................
    # !! IMPORTANT !! There was a case where only 'RGT' and 'Date_time' were returned. I have to include the other 2 columns too by using NA's
    #                 I have to guess the column-names based on the 1st row of the "descr_proc" data

    samp_rownames = as.vector(unlist(descr_proc[1, , drop = T]))
    idx_rgt = which(as.vector(unlist(lapply(gregexpr(pattern = 'RGT', text = samp_rownames), function(x) all(x != -1)))))
    if (length(idx_rgt) == 0) stop("I expect that the RGT column exists!", call. = F)

    idx_date_time = which(as.vector(unlist(lapply(gregexpr(pattern = '-', text = samp_rownames), function(x) all(x != -1)))))
    if (length(idx_date_time) == 0) stop("I expect that the Date-Time column exists!", call. = F)

    colnames(descr_proc)[idx_rgt] = 'RGT'
    colnames(descr_proc)[idx_date_time] = 'Date_time'

    idx_doy = which(as.vector(unlist(lapply(gregexpr(pattern = 'DOY', text = samp_rownames), function(x) all(x != -1)))))
    if (length(idx_doy) == 0) {
      descr_proc$DOY = rep(NA_integer_, nrow(descr_proc))
    }
    else {
      colnames(descr_proc)[idx_doy] = 'DOY'
    }

    idx_cycle = which(as.vector(unlist(lapply(gregexpr(pattern = 'Cycle', text = samp_rownames), function(x) all(x != -1)))))
    if (length(idx_cycle) == 0) {
      descr_proc$cycle = rep(NA_integer_, nrow(descr_proc))
    }
    else {
      colnames(descr_proc)[idx_cycle] = 'cycle'
    }

    # colnames(descr_proc) = c('RGT', 'Date_time', 'DOY', 'cycle')   # this is the order that normally appears in the output data
    #............................................................................................................................................

    if (flag_upper_descr) {
      sf_objs$Description = NULL
    }
    else {
      sf_objs$description = NULL
    }
    
    sf_objs$RGT = as.integer(gsub('RGT ', '', descr_proc$RGT))                                 # keep the integer from the character string which corresponds to the 'RGT'
    sf_objs$Date_time = descr_proc$Date_time
    sf_objs$day_of_year = as.integer(gsub('DOY ', '', descr_proc$DOY))                         # keep the integer from the character string which corresponds to the 'DOY'
    sf_objs$cycle = as.integer(gsub('Cycle ', '', descr_proc$cycle))                           # keep the integer from the character string which corresponds to the 'cycle'
    sf_objs = sf::st_zm(x = sf_objs, drop = T)                                                 # drop the Z dimension

    all_cycles[[CYCLE]] = sf_objs

    if (verbose) cat("The temproary files will be removed ...\n")
    for (tmp_file in lst_kmz) {                                          # remove the .kmz files
      pth_kmz = file.path(tmp_orbits_dir, tmp_file)
      if (file.exists(pth_kmz)) file.remove(pth_kmz)
    }
    if (file.exists(tmp_orbit_file)) file.remove(tmp_orbit_file)         # remove the .zip file

    if (verbose) {
      cat(glue::glue("Processing of cycle '{CYCLE}': "))
      compute_elapsed_time(time_start = t_proc)
    }
  }

  all_cycles = sf::st_as_sf(data.table::rbindlist(all_cycles))
  # all_cycles$Date_time = strptime(x = all_cycles$Date_time, format = "%d-%b-%Y %H:%M:%S")          # convert to Date-Time ('POSIXlt') format  [ !! I have to do that outside the for-loop, otherwise I might receive an error ]
  vec_dates_cycl = as.vector(unlist(lapply(strsplit(all_cycles$Date_time, ' '), function(x) {
    iter_date = strsplit(x[1], '-')[[1]]
    iter_month = switch_abb(iter_date[2])
    as.character(glue::glue("{iter_date[3]}-{iter_month}-{iter_date[1]} {x[2]}"))
  })))
  all_cycles$Date_time = as.POSIXlt(vec_dates_cycl)

  if (verbose) {
    cat("Total ")
    compute_elapsed_time(time_start = t_start)
  }

  return(all_cycles)
}



#' Utilizing Virtual File Systems (vsi) to extract the .kml from the .zip file
#'
#' This function returns the '.kml' and '.kmz' files in form of virtual file paths. Moreover, the user has the option to download these files.
#'
#' @param icesat_rgt_url a character string specifying the input .zip URL
#' @param download_zip a boolean. If TRUE the .zip file will be first downloaded and then the .kml files will be returned, otherwise the 'gdalinfo' function will be used as input to the R 'system2()' function to read the .kml files without downloading the .zip file. The 'gdalinfo' command requires that the user has configured GDAL properly.
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an one column data.table of the output files
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' https://gdal.org/user/virtual_file_systems.html
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom tools file_ext
#' @importFrom sf gdal_utils
#' @importFrom data.table setDT
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#'
#' URL = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/arcticallorbits.zip'
#'
#' #..................................
#' # without downloading the .zip file
#' #..................................
#'
#' res_out = vsi_kml_from_zip(icesat_rgt_url = URL,
#'                            download_zip = FALSE,
#'                            download_method = 'curl',
#'                            verbose = TRUE)
#' str(res_out)
#'
#'
#' #.............................
#' # by downloading the .zip file
#' #.............................
#'
#' res_out = vsi_kml_from_zip(icesat_rgt_url = URL,
#'                            download_zip = TRUE,
#'                            download_method = 'curl',
#'                            verbose = TRUE)
#' str(res_out)
#'
#' }


vsi_kml_from_zip = function(icesat_rgt_url,
                            download_zip = FALSE,
                            download_method = 'curl',
                            verbose = FALSE) {

  if (verbose) t_start = proc.time()
  EXT_url = tools::file_ext(icesat_rgt_url)
  url_pth = as.character(glue::glue('/vsizip/vsicurl/{icesat_rgt_url}'))

  if (download_zip) {

    tmp_url_file = tempfile(fileext = as.character(glue::glue('.{EXT_url}')))

    if (verbose) cat(glue::glue("The '{icesat_rgt_url}' file will be downloaded (temporarily) in the '{tmp_url_file}' file ..."), '\n')
    utils::download.file(url = icesat_rgt_url, destfile = tmp_url_file, method = download_method, quiet = !verbose)
    info_url = utils::unzip(zipfile = tmp_url_file, list = TRUE, junkpaths = T)
    info_url = info_url$Name
    if (length(info_url) == 0) stop(glue::glue("The 'unzip -l' command didn't returned the required .kml url paths for the input url: {url_pth}!"), call. = F)
  }
  else {

    info_url = sf::gdal_utils(util = 'info',
                              source = url_pth,
                              quiet = T)
    if (info_url == "") {
      if (verbose) message("The 'sf' gdalinfo returned an empty character string! Attempt to read the url using the OS configured 'gdalinfo' function ...")

      info_url = suppressWarnings(tryCatch(system2(command = 'gdalinfo', args = url_pth, stdout = TRUE, stderr = FALSE), error = function(e) e))

      if (inherits(info_url, 'error')) stop(glue::glue("The OS 'gdalinfo' gave the following error: {info_url$message}!"), call. = F)
      if (length(info_url) == 0) stop(glue::glue("The OS 'gdalinfo' didn't returned the required .kml url paths for the gdal input url: {url_pth}!"), call. = F)
    }
  }

  out_files = tools::file_ext(info_url)
  tbl_files = data.frame(table(out_files))
  tbl_files = tbl_files[order(tbl_files$Freq, decreasing = T), ]
  type_files = as.character(tbl_files$out_files[1])
  if (verbose) cat(glue::glue("The internal type of the .zip file is '{type_files}'"), '\n')

  idx_kml = which(out_files == type_files)
  if (length(idx_kml) == 0) stop("The .zip file (of the specified 'icesat_rgt_url' parameter) does not include any .kml files!", call. = F)
  info_url = info_url[idx_kml]
  info_url = trimws(info_url, which = 'both')

  macosx_idx = which(gregexpr(pattern = 'MACOSX', text = info_url) != -1)
  LEN_mc = length(macosx_idx)
  if (LEN_mc > 0) {
    if (verbose) message(glue::glue("{LEN_mc} files will be removed because they are hidden MACOSX files!"))
    info_url = info_url[-macosx_idx]
  }

  LEN_url = length(info_url)
  flag_error = F

  if (type_files == 'kml') {
    if (!LEN_url %in% c(1221, 1387)) {
      flag_error = T
      msg = "Except for the 'IS2_RGTs_cycle4_date_time_rev2_0.zip' file which has 1221, I expect that the remaining have 1387 ground track .kml files in every cycle (every cycle consists of approximately 91 days)!"
    }
  }

  if (type_files == 'kmz') {
    if (!LEN_url %in% c(7, 56)) {
      flag_error = T
      msg = "I expect that the 'antarcticaallorbits' and 'arcticallorbits' consist of 7 URLs ( 6 beams + 1 RGT ), whereas the 'ICESat2groundtracksWesternHem' and 'ICESat2groundtracksEasternHem' consist of 56 URLs ( 8 repeats * [6 beams + 1 RGT] )!"
    }
  }

  if (flag_error) stop(msg, call. = F)

  if (verbose) cat(glue::glue("The '{icesat_rgt_url}'  '{EXT_url}'  file includes  {LEN_url}  '{type_files}' files."), '\n')
  if (download_zip) info_url = glue::glue("{url_pth}/{info_url}")
  info_url = data.table::setDT(list(file = info_url))

  if (verbose) compute_elapsed_time(time_start = t_start)

  return(info_url)
}



#' Utilizing Virtual File Systems (vsi) and Well Known Text (WKT) to access the 'nominal orbits'
#'
#' @param orbit_area a character string specifying the earth partition to use, it can be one of 'antarctic', 'arctic', 'western_hemisphere' and 'eastern_hemisphere'
#' @param track a character string specifying the orbit track. Can be one of 'GT1L','GT1R','GT2L','GT2R','GT3L','GT3R' or 'GT7'
#' @param rgt_repeat an integer specifying the orbit repeat. This parameter defaults to 1 and it is relevant if a user chooses one of the 'western_hemisphere' or 'eastern_hemisphere' where there are 8 orbit repeats, whereas for the 'antarctic' and 'arctic' there is only 1 repeat
#' @param wkt_filter either NULL, or a Well Known Text (WKT) character string to allow a user to restrict to an area of interest rather than processing all data (this parameter will be used as input to the 'sf::st_read()' function)
#' @param download_zip a boolean. If TRUE the .zip file will be first downloaded and then the .kml files will be returned, otherwise the 'gdalinfo' function will be used as input to the R 'system2()' function to read the .kml files without downloading the .zip file. The 'gdalinfo' command requires that the user has configured GDAL properly. Set the parameter 'download_zip' to TRUE if GDAL is not (properly) installed.
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an 'sf' object
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' https://gdal.org/user/virtual_file_systems.html
#'
#' @importFrom glue glue
#' @importFrom sf st_layers st_read st_as_sf
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#' require(magrittr)
#'
#' #......................................
#' # processing all data of the orbit area
#' #......................................
#'
#' dat_rgt = vsi_nominal_orbits_wkt(orbit_area = 'eastern_hemisphere',
#'                                  track = 'GT7',
#'                                  rgt_repeat = 1,
#'                                  wkt_filter = NULL,
#'                                  download_method = 'curl',
#'                                  download_zip = FALSE,
#'                                  verbose = TRUE)
#' str(dat_rgt)
#'
#'
#' #...........................................
#' # extracting nominal orbits only for the WKT
#' #...........................................
#'
#' WKT = 'POLYGON ((-14.765 18.979, -11.25 18.979, -11.25 21.943, -14.765 21.943, -14.765 18.979))'
#'
#' dat_rgt = vsi_nominal_orbits_wkt(orbit_area = 'western_hemisphere',
#'                                  track = 'GT3R',
#'                                  rgt_repeat = 8,
#'                                  wkt_filter = WKT,
#'                                  download_method = 'curl',
#'                                  download_zip = FALSE,
#'                                  verbose = TRUE)
#' str(dat_rgt)
#' dat_rgt[[1]]$RGT                 # Reference Ground Tracks of input WKT
#'
#' #.............................
#' # Visualize the results
#' # (first compute the centroid)
#' #.............................
#'
#' wkt_sf = sf::st_as_sfc(WKT, crs = 4326)
#' centr_wkt = sf::st_coordinates(sf::st_centroid(wkt_sf))
#'
#' RGTs = mapview::mapview(dat_rgt, legend = F)
#' AOI_wkt = mapview::mapview(wkt_sf, legend = F)
#'
#' lft = RGTs + AOI_wkt
#' lft@map %>% leaflet::setView(lng = centr_wkt[, 'X'],
#'                              lat = centr_wkt[, 'Y'],
#'                              zoom = 7)
#' }

vsi_nominal_orbits_wkt = function(orbit_area,
                                  track = 'GT7',
                                  rgt_repeat = 1,
                                  wkt_filter = NULL,
                                  download_method = 'curl',
                                  download_zip = FALSE,
                                  verbose = FALSE) {

  if (verbose) t_start = proc.time()
  nomin_orb = available_nominal_orbits(orbit_area = orbit_area, verbose = verbose)
  lst = list()

  for (url_pth in nomin_orb) {

    dat_out = vsi_kml_from_zip(icesat_rgt_url = url_pth,
                               download_zip = download_zip,
                               download_method = download_method,
                               verbose = verbose)
    lst[[url_pth]] = dat_out
  }

  if (verbose) cat('Data based on repeat and track will be kept ...\n')
  dat_item = lapply(lst, function(x) {
    idx = which(gregexpr(pattern = as.character(glue::glue('repeat{rgt_repeat}_{track}')), text = x$file) != -1)
    if (length(idx) > 0) {
      x[idx, , drop = F]
    }
    else {
      NULL
    }
  })

  all_null = all(as.vector(unlist(lapply(dat_item, function(x) is.null(x)))))
  if (all_null) stop("The specified parameter setting (track, rgt_repeat) returned only NULL objects! Be aware: for the 'western_hemisphere' or 'eastern_hemisphere' there is max. 8 repeats, whereas for the 'antarctic' and 'arctic' is only 1 repeat!", call. = F)

  dat_item = data.table::rbindlist(dat_item)
  NROW = nrow(dat_item)

  if (verbose) cat('Data based on repeat and track will be kept ...\n')
  sublists = lapply(1:NROW, function(idx_row) {

    if (verbose) cat(glue::glue("The file '{basename(dat_item$file[idx_row])}' will be processed ..."), '\n')
    lrs = sf::st_layers(dsn = dat_item$file[idx_row])

    NAMS = lrs$name
    if (length(NAMS) == 0) stop(glue::glue("The '{dat_item$file[idx_row]}' does not include any layers!"), call. = F)
    LEN_nams = length(NAMS)

    dat_all = lapply(1:LEN_nams, function(x) {
      LAYER = NAMS[x]
      wkt_inp = character(0)
      if (!is.null(wkt_filter)) wkt_inp = wkt_filter
      lr_dat = sf::st_read(dsn = dat_item$file[idx_row], wkt_filter = wkt_inp, layer = LAYER, quiet = TRUE)
      lr_dat
    })

    dat_all = data.table::rbindlist(dat_all)
    dat_all = sf::st_as_sf(dat_all, crs = 4326)
    dat_all$orbit_area = glue::glue("{rep(basename(dat_item$file[idx_row]), nrow(dat_all))}")
    dat_all = dat_all[, c('Name', 'orbit_area')]
    colnames(dat_all) = c('RGT', 'orbit_area', 'geometry')
    dat_all
  })

  if (length(sublists) > 0) names(sublists) = basename(dat_item$file)

  if (verbose) {
    cat("Total ")
    compute_elapsed_time(time_start = t_start)
  }

  return(sublists)
}



#' Utilizing Virtual File Systems (vsi) and Well Known Text (WKT) to access the 'time specific orbits'
#'
#' @param date_from a character string specifying the 'start' date in the format 'yyyy-MM-dd' (such as '2020-01-01')
#' @param date_to a character string specifying the 'end' date in the format 'yyyy-MM-dd' (such as '2020-01-01')
#' @param RGTs a character vector (consisting of one or more) Reference Ground Track (RGT). See the Examples section on how to come to these RGTs based on the "vsi_nominal_orbits_wkt()" function
#' @param wkt_filter either NULL, or a Well Known Text (WKT) character string to allow a user to restrict to an area of interest rather than processing all data. It is possible that the WKT won't intersect with any of the available time specific orbits due to the sparsity of the coordinates (the output in that case will be an empty list)
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return a list of 'sf' objects where each sublist will represent a different RGT cycle
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' https://gdal.org/user/virtual_file_systems.html
#'
#' @importFrom glue glue
#' @importFrom sf st_layers st_read st_as_sf st_zm
#' @importFrom data.table rbindlist data.table
#' @importFrom utils flush.console
#' @importFrom tools file_ext
#' @importFrom lubridate yday
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' require(IceSat2R)
#' require(magrittr)
#'
#' #...........................................
#' # extracting nominal orbits only for the WKT
#' #...........................................
#'
#' WKT = 'POLYGON ((-14.765 18.979, -11.25 18.979, -11.25 21.943, -14.765 21.943, -14.765 18.979))'
#'
#' dat_rgt = vsi_nominal_orbits_wkt(orbit_area = 'western_hemisphere',
#'                                  track = 'GT3R',
#'                                  rgt_repeat = 8,
#'                                  wkt_filter = WKT,
#'                                  download_method = 'curl',
#'                                  download_zip = FALSE,
#'                                  verbose = TRUE)
#' str(dat_rgt)
#'
#' out_rgt = dat_rgt[[1]]$RGT
#'
#' #.........................................
#' # time specific RGTs (for a time interval)
#' # request using a single RGT cycle
#' #.........................................
#'
#' date_start = '2020-01-01'
#' date_end = '2020-02-01'
#'
#' orb_cyc_single = vsi_time_specific_orbits_wkt(date_from = date_start,
#'                                               date_to = date_end,
#'                                               RGTs = out_rgt,
#'                                               wkt_filter = WKT,
#'                                               verbose = TRUE)
#' str(orb_cyc_single)
#'
#' #.........................................
#' # time specific RGTs (for a time interval)
#' # request using more than one RGT cycles
#' #.........................................
#'
#' date_start = '2019-11-01'
#' date_end = '2020-01-01'
#'
#' orb_cyc_multi = vsi_time_specific_orbits_wkt(date_from = date_start,
#'                                              date_to = date_end,
#'                                              RGTs = out_rgt,
#'                                              wkt_filter = WKT,
#'                                              verbose = TRUE)
#' str(orb_cyc_multi)
#' table(orb_cyc_multi$cycle)
#'
#'
#' #.......................................................
#' # visualization of the output cycles (including the WKT)
#' #.......................................................
#'
#' orb_cyc_multi$cycle = as.factor(orb_cyc_multi$cycle)
#' mp_orb = mapview::mapview(orb_cyc_multi, legend = TRUE, zcol = 'cycle')
#'
#' sf_aoi = sf::st_as_sfc(WKT, crs = 4326)
#' mp_aoi = mapview::mapview(sf_aoi, alpha.regions = 0.3, legend = F)
#'
#' mp_orb + mp_aoi
#'
#' }

vsi_time_specific_orbits_wkt = function(date_from,
                                        date_to,
                                        RGTs,
                                        wkt_filter = NULL,
                                        verbose = FALSE) {
  if (verbose) t_start = proc.time()

  RGTs = as.vector(sapply(RGTs, function(x) {                     # make sure to add the required digits
    if (nchar(x) == 1) x = glue::glue("000{x}")
    else if (nchar(x) == 2) x = glue::glue("00{x}")
    else if (nchar(x) == 3) x = glue::glue("0{x}")
    else x

    as.character(x)
  }))

  avail_rgts = available_RGTs(only_cycle_names = FALSE, verbose = verbose)
  orbit_files = avail_rgts$orbit_files
  orbit_dates = avail_rgts$orbit_dates

  seq_dates = seq(from = as.Date(date_from), to = as.Date(date_to), by = '1 day')
  inters_dates = as.vector(unlist(lapply(orbit_dates, function(x) length(intersect(x = x, y = seq_dates)))))

  res_lst = list()
  idx_inters = which(inters_dates > 0)
  LEN_inters = length(idx_inters)

  if (LEN_inters > 0) {

    cycles_subs = names(orbit_files)[idx_inters]
    zip_subs = as.vector(unlist(orbit_files))[idx_inters]

    if (verbose) {
      cat(glue::glue("In total there are {LEN_inters} intersected dates for which data will be processed!"), '\n')
      cat(glue::glue("The RGT cycles from which data will be processed are: {paste(cycles_subs, collapse = ', ')}"), '\n')
    }

    for (item_cy in seq_along(zip_subs)) {

      iter_cycle = cycles_subs[item_cy]
      if (verbose) {
        cat("-------------------------------------------------\n")
        cat(glue::glue("RGTs of cycle '{iter_cycle}' will be processed ..."), '\n')
        cat("-------------------------------------------------\n")
      }

      iter_url = zip_subs[item_cy]
      zip_dat = vsi_kml_from_zip(icesat_rgt_url = iter_url,
                                 download_zip = FALSE,
                                 download_method = 'curl',
                                 verbose = verbose)
      if (nrow(zip_dat) > 0) {

        greg_expr = as.character(glue::glue("IS2_RGT_{RGTs}"))
        rows_inters = as.vector(sapply(greg_expr, function(x) which(gregexpr(pattern = x, text = zip_dat$file) != -1)))
        if (length(rows_inters)) {
          zip_dat_subs = zip_dat[rows_inters, , drop = F]
          NROW = nrow(zip_dat_subs)

          sublists = lapply(1:NROW, function(idx_row) {

            if (verbose) {
              message(glue::glue("{iter_cycle} FILE: "), idx_row, "/", NROW, "\r", appendLF = FALSE)
              utils::flush.console()
            }

            lrs = sf::st_layers(dsn = zip_dat_subs$file[idx_row])

            NAMS = lrs$name
            if (length(NAMS) == 0) stop(glue::glue("The '{zip_dat_subs$file[idx_row]}' does not include any layers!"), call. = F)
            LEN_nams = length(NAMS)

            dat_all = lapply(1:LEN_nams, function(x) {
              LAYER = NAMS[x]
              wkt_inp = character(0)
              if (!is.null(wkt_filter)) wkt_inp = wkt_filter
              lr_dat = sf::st_read(dsn = zip_dat_subs$file[idx_row], wkt_filter = wkt_inp, layer = LAYER, quiet = TRUE)
              lr_dat
            })

            n_rows = as.vector(unlist(lapply(dat_all, nrow)))
            idx_relev = which(n_rows > 0)

            if (length(idx_relev) > 0) {
              dat_all = dat_all[idx_relev]

              dat_all = data.table::rbindlist(dat_all)

              if (nrow(dat_all) > 0) {
                dat_all
              }
              else {
                NULL
              }
            }
          })

          nams_init = basename(zip_dat_subs$file)                           # 'nams_init' will be modified if there are NULL sublists
          idx_null = which(as.vector(unlist(lapply(sublists, is.null))))
          if (length(idx_null) > 0) {
            if (verbose) message(glue::glue("{length(idx_null)} out of {length(sublists)} sublists were empty and will be removed!"))
            sublists = sublists[-idx_null]
            nams_init = nams_init[-idx_null]
          }

          if (length(sublists) > 0) {
            names(sublists) = nams_init
            res_lst[[iter_cycle]] = sublists
          }
        }
      }
    }
  }

  LEN_lst = length(res_lst)

  if (LEN_lst > 0) {

    nams_out = names(res_lst)
    if (verbose) cat(glue::glue("In total {LEN_lst} RGT cycles will be included in the output 'sf' object ({paste(nams_out, collapse = ', ')})!"), '\n')

    res_lst = lapply(1:LEN_lst, function(y) {

      x = res_lst[[y]]

      nams_lst = names(x)
      unq_ext = unique(tools::file_ext(nams_lst))
      if (length(unq_ext) > 1) stop("I expect either a '.kml' or '.kmz' file extension!", call. = F)

      if (verbose) cat(glue::glue("output of '{nams_out[y]}' will be re-formatted ..."), '\n')

      x = data.table::rbindlist(x)
      x = sf::st_as_sf(x, crs = 4326)              # by default use 'EPSG:4326'

      colnams_sf = colnames(x)
      flag_upper_descr = FALSE
      
      if ('description' %in% colnams_sf) { 
        descr_x = x$description
      }
      else if ('Description' %in% colnams_sf) { 
        descr_x = x$Description
        flag_upper_descr = TRUE
      }
      else {
        stop("The 'D(d)escription' column must exist in the data!", call. = F)
      }
      
      descr_x_invalid = (any(descr_x == "") | any(is.na(descr_x)))

      if (descr_x_invalid) {

        dat_tim = gsub(glue::glue(".{unq_ext}"), '', nams_lst)       # remove the file extension from the file names
        dat_tim = lapply(strsplit(dat_tim, '_'), function(k) {
          iter_date = k[length(k)]
          iter_cycle = k[length(k) - 1]
          iter_cycle = as.integer(gsub('[a-zA-Z]', '', iter_cycle))
          list(date = iter_date, cycle = iter_cycle)
        })
        dat_tim = data.table::rbindlist(dat_tim)
        # yday = strptime(x = dat_tim$date, format = "%d-%b-%Y")                                                    # this command might not work for specific system locale
        yday = as.vector(unlist(lapply(strsplit(dat_tim$date, '-'), function(x) {
          iter_month = switch_abb(x[2])
          as.character(glue::glue("{x[3]}-{iter_month}-{x[1]}"))
        })))

        dat_tim$date = as.character(glue::glue("{dat_tim$date} 00:00:00"))       # add time so that it matches the next "else" condition time-format

        if (flag_upper_descr) {
          x$Description = NULL
        }
        else {
          x$description = NULL
        }
        
        x$RGT = as.integer(gsub('RGT ', '', x$Name))
        x$Date_time = dat_tim$date                                               # add the date
        x$day_of_year = lubridate::yday(x = yday)
        x$cycle = dat_tim$cycle
      }
      else {
        if (verbose) cat("The 'description' column of the output data will be processed ...\n")
        
        colnams_dtbl = colnames(x)
        if ('description' %in% colnams_dtbl) {                                 # see previous exception regarding "D(d)escription" (here I assume that only "description" or "Description" cases exist)
          x$description = as.character(x$description)
          descr_proc = strsplit(x = x$description, split = '\n')
        }
        else {
          x$Description = as.character(x$Description)
          descr_proc = strsplit(x = x$Description, split = ' ')          # in case that I have a 'Description' column, split by empty space
          chk_row_items = as.vector(unlist(lapply(descr_proc, function(x) (length(x) %% 2) == 0)))     # check that I have an even number of columns (normally 8 but can be fewer too) then concatenate the columns by pairs of consecutive items
          if (!all(chk_row_items)) stop("It seems that after splitting the observations by empty space the number of columns (per row) are not an even number (vsi function)!", call. = F)
          descr_proc = lapply(descr_proc, function(x) {
            seq_item = seq(from = 1, to = length(x), by = 2)
            sapply(seq_item, function(y) paste(c(x[y], x[y+1]), collapse = ' '))
          })
        }
        
        descr_proc = data.table::data.table(do.call(rbind, descr_proc), stringsAsFactors = F)
        colnames(descr_proc) = c('RGT', 'Date_time', 'DOY', 'cycle')

        if (flag_upper_descr) {
          x$Description = NULL
        }
        else {
          x$description = NULL
        }
        
        x$RGT = as.integer(gsub('RGT ', '', descr_proc$RGT))                           # keep the integer from the character string which corresponds to the 'RGT'
        x$Date_time = descr_proc$Date_time
        x$day_of_year = as.integer(gsub('DOY ', '', descr_proc$DOY))                   # keep the integer from the character string which corresponds to the 'DOY'
        x$cycle = as.integer(gsub('Cycle ', '', descr_proc$cycle))                     # keep the integer from the character string which corresponds to the 'cycle'
      }

      x = sf::st_zm(x = x, drop = T)                                                   # drop the Z dimension
      x
    })

    res_lst = sf::st_as_sf(data.table::rbindlist(res_lst))
    # res_lst$Date_time = strptime(x = res_lst$Date_time, format = "%d-%b-%Y %H:%M:%S")          # convert to Date-Time ('POSIXlt') format  [ !! I have to do that outside the for-loop, otherwise I might receive an error ]

    vec_lst_cycl = as.vector(unlist(lapply(strsplit(res_lst$Date_time, ' '), function(x) {
      iter_date = strsplit(x[1], '-')[[1]]
      iter_month = switch_abb(iter_date[2])
      as.character(glue::glue("{iter_date[3]}-{iter_month}-{iter_date[1]} {x[2]}"))
    })))
    res_lst$Date_time = as.POSIXlt(vec_lst_cycl)
  }
  else {
    if (verbose) message(glue::glue("The specified parameter setting returned an empty list for all {length(idx_inters)} processed cycles!"))
  }

  if (verbose) {
    cat("Total ")
    compute_elapsed_time(time_start = t_start)
  }

  return(res_lst)
}

