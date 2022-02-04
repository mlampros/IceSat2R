
utils::globalVariables(c('i'))                        # for the foreach for-loop


#' Overall Mission Orbits
#'
#' @param orbit_area a character string specifying the nominal mission orbits and beam locations. It can be one of 'antarctic', 'arctic', 'western_hemisphere' or 'eastern_hemisphere'
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param threads an integer that specifies the number of threads to use in parallel when processing the data
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an 'sf' object of multiple tracks (see the 'LAYER' column of the output object)
#'
#' @details
#'
#' These files contain the nominal mission orbits and beam locations, the files have 7 tracks per orbit: one for each of the six beams
#' of ICESat-2, and the seventh for the Reference Ground Track (RGT). The RGT is an imaginary line through the six-beam pattern that is
#' handy for getting a sense of where the orbits fall on Earth, and which the mission uses to point the observatory. However, the six
#' tracks for the six beams are our best estimate of where the beams will actually fall on Earth's surface.
#'
#' @references
#'
#' https://icesat-2.gsfc.nasa.gov/science/specs
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip globalVariables
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

  orbits = list(antarctic = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/antarcticaallorbits.zip',                          # 'antarctic_orbit'
                arctic = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/arcticallorbits.zip',                                 # 'arctic_orbit'
                western_hemisphere = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2groundtracksWesternHem.zip',       # 'western_hemisphere_ground_tracks'
                eastern_hemisphere = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/ICESat2groundtracksEasternHem.zip')       # 'eastern_hemisphere_ground_tracks'

  tmp_orbit_file = tempfile(fileext = '.zip')
  if (verbose) cat(glue::glue("The orbits of '{orbit_area}' will be downloaded in the '{tmp_orbit_file}' file ..."), '\n')
  utils::download.file(url = orbits[[orbit_area]], destfile = tmp_orbit_file, method = download_method, quiet = !verbose)
  tmp_orbits_dir = file.path(tempdir(), glue::glue('orbits_{orbit_area}'))                                                  # create a second directory inside the temporary directory to extract the .zip file
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
#' @param only_cycle_names a boolean. If TRUE then only the RGT (Reference Ground Track) cycle names will be returned. Otherwise all orbit files, dates and cycle names.
#'
#' @return a list object with the available orbit files, dates and cycle names
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
#' #.................................................
#' # all available orbit files, dates and cycle names
#' #.................................................
#'
#' avail_dat = available_RGTs(only_cycle_names = FALSE)
#' avail_dat
#'
#' #.............................
#' # receive only the cycle names
#' #.............................
#'
#' avail_cycles = available_RGTs(only_cycle_names = TRUE)
#' avail_cycles


available_RGTs = function(only_cycle_names = FALSE) {

  orbit_files = list(RGT_cycle_1 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle1_date_time.zip',
                     RGT_cycle_2 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle2_date_time.zip',
                     RGT_cycle_3 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle3_date_time_0.zip',
                     RGT_cycle_4 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle4_date_time_rev2_0.zip',
                     RGT_cycle_5 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle5_date_time_rev3.zip',
                     RGT_cycle_6 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle6_date_time.zip',
                     RGT_cycle_7 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle7_date_time.zip',
                     RGT_cycle_8 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle8_date_time.zip',
                     RGT_cycle_9 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle9_date_time.zip',
                     RGT_cycle_10 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle10_date_time.zip',
                     RGT_cycle_11 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle11_date_time.zip',
                     RGT_cycle_12 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2RGTscycle12datetime.zip',
                     RGT_cycle_13 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle13_date_time.zip',
                     RGT_cycle_14 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle14_date_time.zip',
                     RGT_cycle_15 = 'https://icesat-2.gsfc.nasa.gov/sites/default/files/page_files/IS2_RGTs_cycle15_date_time.zip')

  orbit_dates = list(RGT_cycle_1 = seq(from = as.Date('2018-10-13'), to = as.Date('2018-12-28'), by = '1 day'),
                     RGT_cycle_2 = seq(from = as.Date('2018-12-28'), to = as.Date('2019-03-29'), by = '1 day'),
                     RGT_cycle_3 = seq(from = as.Date('2019-03-29'), to = as.Date('2019-06-28'), by = '1 day'),
                     RGT_cycle_4 = seq(from = as.Date('2019-07-09'), to = as.Date('2019-09-26'), by = '1 day'),
                     RGT_cycle_5 = seq(from = as.Date('2019-09-26'), to = as.Date('2019-12-26'), by = '1 day'),
                     RGT_cycle_6 = seq(from = as.Date('2019-12-26'), to = as.Date('2020-03-26'), by = '1 day'),
                     RGT_cycle_7 = seq(from = as.Date('2020-03-26'), to = as.Date('2020-06-25'), by = '1 day'),
                     RGT_cycle_8 = seq(from = as.Date('2020-06-25'), to = as.Date('2020-09-24'), by = '1 day'),
                     RGT_cycle_9 = seq(from = as.Date('2020-09-24'), to = as.Date('2020-12-23'), by = '1 day'),
                     RGT_cycle_10 = seq(from = as.Date('2020-12-24'), to = as.Date('2021-03-24'), by = '1 day'),
                     RGT_cycle_11 = seq(from = as.Date('2021-03-24'), to = as.Date('2021-06-23'), by = '1 day'),
                     RGT_cycle_12 = seq(from = as.Date('2021-06-23'), to = as.Date('2021-09-22'), by = '1 day'),
                     RGT_cycle_13 = seq(from = as.Date('2021-09-22'), to = as.Date('2021-12-22'), by = '1 day'),
                     RGT_cycle_14 = seq(from = as.Date('2021-12-22'), to = as.Date('2022-03-23'), by = '1 day'),
                     RGT_cycle_15 = seq(from = as.Date('2022-03-23'), to = as.Date('2022-06-21'), by = '1 day'))

  orb_nams = names(orbit_files)
  orb_dats = names(orbit_dates)
  if (any(orb_nams != orb_dats)) stop("There is a mismatch between the names of the 'orbit_files' and 'orbit_dates'!", call. = F)

  if (only_cycle_names) {
    return(names(orbit_files))
  }
  else {
    return(list(orbit_files = orbit_files,
                orbit_dates = orbit_dates,
                names_cycles = orb_nams))
  }
}



#' Revisit Time Reference Ground Tracks and Dates
#'
#' @param RGT_cycle either NULL or a character string specifying a single RGT (Reference Ground Track) as determined by the output of the 'available_RGTs(only_cycle_names = TRUE)' function. If NULL then all available Data will be returned.
#' @param complete_date_sequence a boolean. If TRUE a complete sequence of Dates will be returned, otherwise only the 'minimum' and 'maximum' Dates.
#'
#' @return a list object with the available orbit files, dates and date sequence lengths
#'
#' @details
#'
#' This function returns the .zip (kmz files) for all of the laser tracks over each 91-day repeat period (revisit time). Note that the locations and times are estimates, but
#' should be correct to within a few minutes in time and better than 100m in the predicted locations.
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
#' avail_cycles = available_RGTs(only_cycle_names = TRUE)
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
#' @param date_from either NULL or a character string specifying the start date in the format 'yyyy-MM-dd' (such as '2020-01-01'). If this parameter is NULL then the 'RGT_cycle' parameter must be specified
#' @param date_to either NULL or a character string specifying the end date in the format 'yyyy-MM-dd' (such as '2020-01-01'). If this parameter is NULL then the 'RGT_cycle' parameter must be specified
#' @param RGT_cycle a character vector specifying the RGT (Reference Ground Track) cycle(s) (the specific revisit times of ICESAT-2). This parameter can be greater or equal to 1 with a maximum 'RGT-cycle' names as determined by the output of the 'available_RGTs(only_cycle_names = TRUE)' function. The computation time of a single 'RGT-cycle' might take approximately 15 minutes utilizing 8 threads (in parallel) and require approximately 2 GB of memory.
#' @param download_method a character string specifying the download method. Corresponds to the 'method' parameter of the 'utils::download.file()' function. Can be one of 'internal', 'wininet' (Windows only), 'libcurl', 'wget', 'curl' or 'auto'
#' @param threads an integer that specifies the number of threads to use in parallel when processing the data
#' @param verbose a boolean. If TRUE then information will be printed out in the console
#'
#' @return an 'sf' object that will include one or more Reference Ground Tracks  (see the 'RGT' column of the output object)
#'
#' @details
#'
#' These files contain the reference ground track time and locations for specific date ranges. Updated KML files have been posted to the 'tech-specs'
#' website (see the 'references' section for more details) containing individual files for each RGT with a date and time stamp posted every 420 kilometers
#' along-track (roughly 1 minute of flight time in between each point). The first RGT is 234; this is where the time series begins. The date of each RGT is
#' in the file name, so the user can easily ascertain where and when ICESat-2 will be on a particular day.
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
#'
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
#' avail_cycles = available_RGTs(only_cycle_names = TRUE)
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

  avail_rgts = available_RGTs(only_cycle_names = FALSE)
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
    vec_dates_files = as.Date(strptime(x = vec_dates_files, format = "%d-%b-%Y"))
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
          lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)
          lr_dat
        }
      }
      else {
        inner_obj = lapply(1:LEN_nams, function(x) {
          LAYER = NAMS[x]
          lr_dat = sf::st_read(dsn = iter_file, layer = LAYER, quiet = T)
          lr_dat
        })
      }

      inner_obj = sf::st_as_sf(data.table::rbindlist(inner_obj))
      sf_objs[[glue::glue("{nam_file}")]] = inner_obj
    }

    sf_objs = sf::st_as_sf(data.table::rbindlist(sf_objs))

    if (verbose) cat("The 'description' column of the output data will be processed ...\n")
    descr_proc = strsplit(x = sf_objs$description, split = '\n')
    descr_proc = data.table::data.table(do.call(rbind, descr_proc), stringsAsFactors = F)
    colnames(descr_proc) = c('RGT', 'Date_time', 'DOY', 'cycle')

    sf_objs$description = NULL
    sf_objs$RGT = as.integer(gsub('RGT ', '', descr_proc$RGT))                                 # keep the integer from the character string which corresponds to the 'RGT'
    sf_objs$Date_time = descr_proc$Date_time
    sf_objs$day_of_year = as.integer(gsub('DOY ', '', descr_proc$DOY))                                 # keep the integer from the character string which corresponds to the 'DOY'
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
  all_cycles$Date_time = strptime(x = all_cycles$Date_time, format = "%d-%b-%Y %H:%M:%S")          # convert to Date-Time ('POSIXlt') format  [ !! I have to do that outside the for-loop, otherwise I might receive an error ]

  if (verbose) {
    cat("Total ")
    compute_elapsed_time(time_start = t_start)
  }

  return(all_cycles)
}

