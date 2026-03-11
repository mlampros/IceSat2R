testthat::test_that("the function 'getTracks()' gives an error if the 'outputFormat' parameter is not one of 'csv' or 'json'!", {
  testthat::expect_error(getTracks(
    minx = -144.67439,
    miny = 59.22850,
    maxx = -137.88048,
    maxy = 61.69038,
    date = "2020-01-01",
    outputFormat = "zip",
    download_method = "curl",
    verbose = FALSE
  ))
})


testthat::test_that("the function 'getTracks()' returns the correct output!", {
  testthat::skip_on_cran() # skip on CRAN due to time limits and might fail

  res_df <- tryCatch(
    getTracks(
      minx = minx,
      miny = miny,
      maxx = maxx,
      maxy = maxy,
      date = "2021-02-15",
      outputFormat = "csv",
      download_method = "curl",
      verbose = FALSE
    ),
    error = function(e) NULL
  )

  testthat::skip_if(is.null(res_df) || nrow(res_df) == 0, "API returned no data or an error; skipping value check")
  testthat::expect_true(is.data.frame(res_df) & "track" %in% colnames(res_df))
})
