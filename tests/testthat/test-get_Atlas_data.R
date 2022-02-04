
context("icesat-2 atlas data R file")


#............................
# "get_atlas_data()" function
#............................


testthat::test_that("the function 'get_atlas_data()' gives an error in case the product is 'atl03' and the 'beamName' parameter is set to NULL!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = NULL,
                                        product = 'atl03'))
})


testthat::test_that("the function 'get_atlas_data()' gives an error if the 'beamName' parameter is invalid!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = 'INVALID',
                                        product = 'atl06'))
})


testthat::test_that("the function 'get_atlas_data()' gives an error if the 'product' parameter is not one of 'atl03', 'atl06', 'atl07', 'atl08', 'atl10', 'atl12', 'atl13'!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = 'gt1l',
                                        product = 'INVALID'))
})


testthat::test_that("the function 'get_atlas_data()' gives an error if the 'client' parameter is not one of 'portal', 'jupyter'!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = 'gt1l',
                                        product = 'atl06',
                                        client = 'INVALID'))
})


testthat::test_that("the function 'get_atlas_data()' gives an error if the 'outputFormat' parameter is not one of 'csv', 'json', 'zip'!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = 'gt1l',
                                        product = 'atl06',
                                        client = 'portal',
                                        outputFormat = 'INVALID'))
})


testthat::test_that("the function 'get_atlas_data()' gives an error if the 'product' parameter is 'atl03' and the 'photonConfidence' is other than NULL, 'noise', 'buffer', 'low', 'medium' or 'high'!", {

  testthat::expect_error(get_atlas_data(minx = -144.67439,
                                        miny = 59.22850,
                                        maxx = -137.88048,
                                        maxy = 61.69038,
                                        date = '2020-01-01',
                                        trackId = 1290,
                                        beamName = 'gt1l',
                                        product = 'atl03',
                                        photonConfidence = 'INVALID',
                                        client = 'portal',
                                        outputFormat = 'INVALID'))
})



#..............................
# "get_level3a_data()" function
#..............................


testthat::test_that("the function 'get_level3a_data()' gives an error in case the product is not one of 'atl06', 'atl07', 'atl08', 'atl10', 'atl12', 'atl13'!", {

  testthat::expect_error(get_level3a_data(minx = -144.67439,
                                          miny = 59.22850,
                                          maxx = -137.88048,
                                          maxy = 61.69038,
                                          startDate = '2020-01-01',
                                          endDate = '2020-01-01',
                                          trackId = 1290,
                                          beamName = NULL,
                                          product = 'atl03'))
})


testthat::test_that("the function 'get_level3a_data()' gives an error if the 'client' parameter is not one of 'portal', 'jupyter'!", {

  testthat::expect_error(get_level3a_data(minx = -144.67439,
                                          miny = 59.22850,
                                          maxx = -137.88048,
                                          maxy = 61.69038,
                                          startDate = '2020-01-01',
                                          endDate = '2020-01-03',
                                          trackId = 1290,
                                          beamName = NULL,
                                          product = 'atl06',
                                          client = 'INVALID'))
})


testthat::test_that("the function 'get_level3a_data()' gives an error if the 'outputFormat' parameter is not one of 'csv', 'json', 'zip'!", {

  testthat::expect_error(get_level3a_data(minx = -144.67439,
                                          miny = 59.22850,
                                          maxx = -137.88048,
                                          maxy = 61.69038,
                                          startDate = '2020-01-01',
                                          endDate = '2020-01-03',
                                          trackId = 1290,
                                          beamName = NULL,
                                          product = 'atl06',
                                          client = 'portal',
                                          outputFormat = 'INVALID'))
})


testthat::test_that("the function 'get_level3a_data()' gives an error if the 'startDate' parameter is not equal or smaller to the 'endDate' parameter!", {

  testthat::expect_error(get_level3a_data(minx = -144.67439,
                                          miny = 59.22850,
                                          maxx = -137.88048,
                                          maxy = 61.69038,
                                          startDate = '2020-01-02',
                                          endDate = '2020-01-01',
                                          trackId = 1290,
                                          beamName = NULL,
                                          product = 'atl06',
                                          client = 'portal',
                                          outputFormat = 'csv'))
})


testthat::test_that("the function 'get_level3a_data()' gives an error if the 'beamName' parameter is invalid!", {

  testthat::expect_error(get_level3a_data(minx = -144.67439,
                                          miny = 59.22850,
                                          maxx = -137.88048,
                                          maxy = 61.69038,
                                          startDate = '2020-01-01',
                                          endDate = '2020-01-03',
                                          trackId = 1290,
                                          beamName = 'INVALID',
                                          product = 'atl06',
                                          client = 'portal',
                                          outputFormat = 'csv'))
})

