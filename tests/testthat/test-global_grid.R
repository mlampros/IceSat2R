
testthat::test_that("the function 'degrees_to_global_grid()' gives the correct output either for 1- or for a 5-degrees grid!", {

  glob_grid_1_degr = degrees_to_global_grid(minx = minx,
                                            miny = miny,
                                            maxx = maxx,
                                            maxy = maxy,
                                            degrees = 1.0,
                                            square_geoms = TRUE,
                                            crs_value = 4326,
                                            verbose = FALSE)

  glob_grid_5_degr = degrees_to_global_grid(minx = minx,
                                            miny = miny,
                                            maxx = maxx,
                                            maxy = maxy,
                                            degrees = 5.0,
                                            square_geoms = TRUE,
                                            crs_value = 4326,
                                            verbose = FALSE)

  testthat::expect_true(nrow(glob_grid_1_degr) == 25 & ncol(glob_grid_1_degr) == 2 & nrow(glob_grid_5_degr) == 1 & ncol(glob_grid_5_degr) == 2)
})
