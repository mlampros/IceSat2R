
testthat::test_that("the function 'revisit_time_RGTs()' gives an error if the 'RGT_cycle' parameter is invalid!", {

  testthat::expect_error(revisit_time_RGTs(RGT_cycle = 'INVALID'))
})


testthat::test_that("the function 'time_specific_orbits()' gives an error if all 3 parameters ('date_from', 'date_to', 'RGT_cycle') are specified!", {

  testthat::expect_error(time_specific_orbits(date_from = '2020-01-01',
                                              date_to = '2020-01-15',
                                              RGT_cycle = 'RGT_cycle_6'))
})


testthat::test_that("the function 'time_specific_orbits()' gives an error in case that the 'RGT_cycle' parameter is NULL and also any of ('date_from', 'date_to') parameters are NULL too!", {

  testthat::expect_error(time_specific_orbits(date_from = NULL,
                                              date_to = '2020-01-15',
                                              RGT_cycle = NULL))
})


testthat::test_that("the function 'time_specific_orbits()' gives an error if the 'date_from' is greater than the 'date_to' parameter!", {

  testthat::expect_error(time_specific_orbits(date_from = '2020-01-15',
                                              date_to = '2020-01-01',
                                              RGT_cycle = NULL))
})


testthat::test_that("the function 'latest_orbits()' returns the expected output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  df_orbs = latest_orbits(technical_specs_url = "https://icesat-2.gsfc.nasa.gov/science/specs", verbose = FALSE)
  df_orbs_spl = split(df_orbs, by = 'Type')

  testthat::expect_true(ncol(df_orbs) == 5 & nrow(df_orbs) >= 16 & !any(is.na(c(df_orbs_spl$Time_Specific$Date_from, df_orbs_spl$Time_Specific$Date_to))))
})


testthat::test_that("the function 'available_nominal_orbits()' returns the expected output (orbit_area is NULL)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  nomin_orb = available_nominal_orbits(verbose = FALSE)

  testthat::expect_true(inherits(nomin_orb, 'list') & length(nomin_orb) > 0)
})


testthat::test_that("the function 'available_nominal_orbits()' returns the expected output (orbit_area is 'arctic')!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  nomin_orb = available_nominal_orbits(orbit_area = 'arctic', verbose = FALSE)

  testthat::expect_true(inherits(nomin_orb, 'list') & length(nomin_orb) == 1)
})


# #................................................................................................. it takes approximately 1 minute using 1 thread
testthat::test_that("the function 'overall_mission_orbits()' returns the expected output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  
  if (Sys.info()["sysname"] == 'Linux') {         # skip for Windows, Mac OSX because it seems that the gdal version can not read .kmz files and throws an error
    
    res_orb  = overall_mission_orbits(orbit_area = 'eastern_hemisphere',
                                      download_method = 'curl',
                                      threads = 1,
                                      verbose = FALSE)
    
    testthat::expect_true(inherits(res_orb, c("sf", "data.table", "data.frame")) & nrow(res_orb) > 1 & any(c("Description", "description") %in% colnames(res_orb)))
  }
})
# #.................................................................................................


testthat::test_that("the function 'available_RGTs()' returns the expected output (only_cycle_names is FALSE)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  rgt_lst = available_RGTs(only_cycle_names = FALSE, verbose = FALSE)

  testthat::expect_true(inherits(rgt_lst, 'list') & length(rgt_lst) == 3 & all(names(rgt_lst) %in% c("orbit_files", "orbit_dates", "names_cycles")) & all(unlist(lapply(rgt_lst, function(x) length(x) > 0))))
})


testthat::test_that("the function 'available_RGTs()' returns the expected output (only_cycle_names is TRUE)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  rgt_lst = available_RGTs(only_cycle_names = TRUE, verbose = FALSE)

  testthat::expect_true(inherits(rgt_lst, 'character') & length(rgt_lst) > 0)
})


testthat::test_that("the function 'revisit_time_RGTs()' returns the expected output (RGT_cycle is NULL and complete_date_sequence is TRUE)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  rev_all = revisit_time_RGTs(RGT_cycle = NULL, complete_date_sequence = TRUE)

  testthat::expect_true(inherits(rev_all, 'list') & length(rev_all) == 3 & all(names(rev_all) %in% c("orbit_files", "orbit_dates", "length_dates")) & all(unlist(lapply(rev_all, function(x) length(x) >= 1))))
})


testthat::test_that("the function 'revisit_time_RGTs()' returns the expected output (RGT_cycle is 'RGT_cycle_1' and complete_date_sequence is FALSE)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  rev_all = revisit_time_RGTs(RGT_cycle = 'RGT_cycle_1', complete_date_sequence = FALSE)

  testthat::expect_true(inherits(rev_all, 'list') & length(rev_all) == 3 & all(names(rev_all) %in% c("orbit_files", "orbit_dates", "length_dates")) & all(unlist(lapply(rev_all, function(x) length(x) >= 1))))
})


# #................................................................................................. it takes approximately 2 minutes using 1 thread
testthat::test_that("the function 'time_specific_orbits()' returns the expected output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  res_rgt_one = time_specific_orbits(date_from = '2019-06-01',
                                     date_to = '2019-06-01',
                                     download_method = 'curl',
                                     threads = 1,
                                     verbose = FALSE)

  testthat::expect_true(inherits(res_rgt_one, c("sf", "data.table", "data.frame")) & nrow(res_rgt_one) > 0 & all(c("Name", "RGT", "Date_time", "day_of_year", "cycle", "geometry") %in% colnames(res_rgt_one)) & !any(is.na(res_rgt_one$Date_time)))
})
# #.................................................................................................


testthat::test_that("the function 'vsi_kml_from_zip()' returns the expected output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  testthat::skip_on_ci()           # skip on Github Actions which throws an error when using the '/vsi../' url  [ for the 'skip_on_ci()' see: https://github.com/r-lib/testthat/issues/970#issuecomment-611176719 AND https://github.com/r-lib/actions/issues/81 ]

  res_out = vsi_kml_from_zip(icesat_rgt_url = ORBITS_URL,
                             download_zip = FALSE,             # Don't download the file which takes too long
                             download_method = 'curl',
                             verbose = FALSE)

  testthat::expect_true(inherits(res_out, c('data.table', 'data.frame')) & nrow(res_out) > 0 & ncol(res_out) == 1)
})


testthat::test_that("the function 'vsi_nominal_orbits_wkt()' returns the expected output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  testthat::skip_on_ci()           # skip on Github Actions which throws an error when using the '/vsi../' url  [ for the 'skip_on_ci()' see: https://github.com/r-lib/testthat/issues/970#issuecomment-611176719 AND https://github.com/r-lib/actions/issues/81 ]

  dat_rgt = vsi_nominal_orbits_wkt(orbit_area = 'eastern_hemisphere',
                                   track = 'GT7',
                                   rgt_repeat = 1,
                                   wkt_filter = NULL,
                                   download_method = 'curl',
                                   download_zip = FALSE,
                                   verbose = FALSE)

  testthat::expect_true(length(dat_rgt) == 1 & inherits(dat_rgt[[1]], c("sf", "data.table", "data.frame")) & nrow(dat_rgt[[1]]) == 1478 & ncol(dat_rgt[[1]]) == 3)
})


testthat::test_that("the function 'vsi_nominal_orbits_wkt()' returns the expected output (extracting nominal orbits only for a WKT)!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  testthat::skip_on_ci()           # skip on Github Actions which throws an error when using the '/vsi../' url  [ for the 'skip_on_ci()' see: https://github.com/r-lib/testthat/issues/970#issuecomment-611176719 AND https://github.com/r-lib/actions/issues/81 ]

  dat_rgt = vsi_nominal_orbits_wkt(orbit_area = 'western_hemisphere',
                                   track = 'GT3R',
                                   rgt_repeat = 8,
                                   wkt_filter = WKT,
                                   download_method = 'curl',
                                   download_zip = FALSE,
                                   verbose = FALSE)

  testthat::expect_true(length(dat_rgt) == 1 & inherits(dat_rgt[[1]], c("sf", "data.table", "data.frame")) & nrow(dat_rgt[[1]]) == 30 & ncol(dat_rgt[[1]]) == 3)
})


testthat::test_that("the function 'vsi_time_specific_orbits_wkt()' returns the expected output for an input WKT!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  testthat::skip_on_ci()           # skip on Github Actions which throws an error when using the '/vsi../' url  [ for the 'skip_on_ci()' see: https://github.com/r-lib/testthat/issues/970#issuecomment-611176719 AND https://github.com/r-lib/actions/issues/81 ]

  orb_cyc_single = vsi_time_specific_orbits_wkt(date_from = date_start,
                                                date_to = date_end,
                                                RGTs = sample_rgts,
                                                wkt_filter = WKT,
                                                verbose = FALSE)

  testthat::expect_true(inherits(orb_cyc_single, c("sf", "data.table", "data.frame")) & nrow(orb_cyc_single) == 2 & all(c("Name", "RGT", "Date_time", "day_of_year", "cycle", "geometry") %in% colnames(orb_cyc_single)))
})


testthat::test_that("the function 'time_specific_orbits()' returns the expected output for an input date range!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  approx_date_start = "2021-02-01"
  approx_date_end = "2021-02-15"

  res_rgt_many = time_specific_orbits(date_from = approx_date_start,
                                      date_to = approx_date_end,
                                      RGT_cycle = NULL,
                                      download_method = 'curl',
                                      threads = 1,
                                      verbose = FALSE)

  testthat::expect_true( inherits(res_rgt_many, c("sf", "data.table", "data.frame")) & nrow(res_rgt_many) > 0 & all(c("Name", "RGT", "Date_time", "day_of_year", "cycle", "geometry") %in% colnames(res_rgt_many)))
})
