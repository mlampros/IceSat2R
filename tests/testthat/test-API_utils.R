
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

  res_df = get_URL_data(URL = VALID_URL_remaining,
                        outputFormat = 'csv',
                        download_method = 'curl',
                        file_path_zip = NULL,
                        verbose = FALSE)

  testthat::expect_true(nrow(res_df) == 4 & ncol(res_df) == 11)
})

