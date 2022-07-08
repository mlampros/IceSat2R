
testthat::test_that("the 'select_aoi_global_grid()' R6 class and especially the 'draw_edit_aoi()' method gives an error if the 'area_of_interest' parameter is neither a character string nor an input object of class 'bbox'!", {

  testthat::skip_if_not(interactive())        # skip if not interactive because it might give the following NOTE on CRAN: "Check: for detritus in the temp directory Found the following files/directories: 'Crashpad'" (see also: https://stackoverflow.com/a/62456524)

  init = select_aoi_global_grid$new(area_of_interest = list(), verbose = FALSE)

  testthat::expect_error(init$draw_edit_aoi(degrees = 1.0))
})

