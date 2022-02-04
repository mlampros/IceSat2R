

context("icesat-2 tracks data R file")


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

