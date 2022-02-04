

context("mission orbits R file")


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
