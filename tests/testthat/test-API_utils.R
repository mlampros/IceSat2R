
testthat::test_that("the function 'get_URL_data()' gives an error if the input url is invalid!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  testthat::expect_error(get_URL_data(URL = INVALID_URL,
                                      outputFormat = 'csv',
                                      download_method = 'curl',
                                      file_path_zip = NULL,
                                      verbose = FALSE))
})


testthat::test_that("the function 'get_URL_data()' returns a data.frame if the input url is valid!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail
  
  expected_COLS = c("segment_id_beg", "segment_id_end", "longitude", "latitude", 
                    "h_te_best_fit", "h_te_uncertainty", "h_canopy", "h_canopy_uncertainty", 
                    "track_id", "beam", "file_name")

  res_df = get_URL_data(URL = VALID_URL_remaining,
                        outputFormat = 'csv',
                        download_method = 'curl',
                        file_path_zip = NULL,
                        verbose = FALSE)
  
  cols_df = colnames(res_df)

  testthat::expect_true(all(cols_df %in% expected_COLS) & ncol(res_df) == length(cols_df))
})


testthat::test_that("the function 'verify_RGTs()' gives an error if the input bounding box is invalid!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  testthat::expect_error(verify_RGTs(nsidc_rgts = rgts, bbx_aoi = c(xmin = -53.108876,
                                                                    ymin = 60.119614,
                                                                    xmax = -19.203521,
                                                                    INVALID = 80.793117), verbose = TRUE))
})


testthat::test_that("the function 'verify_RGTs()' returns a data.table if the input parameters are valid!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  dtbl = verify_RGTs(nsidc_rgts = rgts, bbx_aoi = bbx, verbose = FALSE)

  testthat::expect_true(nrow(dtbl) == 6 & ncol(dtbl) == 3 & all(colnames(dtbl) %in% c("Date_time", "RGT_OpenAlt", "RGT_NSIDC")))
})
