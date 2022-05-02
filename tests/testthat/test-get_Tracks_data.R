
testthat::test_that("the function 'getTracks()' gives an error if the 'outputFormat' parameter is not one of 'csv' or 'json'!", {

  testthat::expect_error(getTracks(minx = -144.67439,
                                   miny = 59.22850,
                                   maxx = -137.88048,
                                   maxy = 61.69038,
                                   date = '2020-01-01',
                                   outputFormat = 'zip',
                                   download_method = 'curl',
                                   verbose = FALSE))
})


testthat::test_that("the function 'getTracks()' returns the correct output!", {

  testthat::skip_on_cran()         # skip on CRAN due to time limits and might fail

  res_df = getTracks(minx = minx,
                     miny = miny,
                     maxx = maxx,
                     maxy = maxy,
                     date = "2021-02-15",
                     outputFormat = 'csv',
                     download_method = 'curl',
                     verbose = FALSE)

  testthat::expect_true(nrow(res_df) == 1 & res_df$track == 817)
})
