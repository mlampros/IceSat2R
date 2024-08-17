
## IceSat2R 1.0.6

* I updated the vignettes of the "IceSat2R" package due to errors in the "r-oldrel-macos" CRAN versions


## IceSat2R 1.0.5

* I adjusted the "IceSat-2_Virtual_File_System_Orbits_PDF.Rmd" and "IceSat-2_Virtual_File_System_Orbits_HTML.Rmd" vignettes to fix the "Error in CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),: Feature creation failed." error
* I renamed the Dockerfiles and I added a separate Dockerfile for the IceSatR package because the docker image didn't work due to similar naming with the binder image. I also added (in the Dockerfiles) the "awscli" installation which is required for the vignettes. The ".github/workflows/docker_image.yml" file was adjusted as well.
* I updated the documentation to account for the migration of OpenAltimetry to EarthData (https://openaltimetry.earthdatacloud.nasa.gov/). The new OpenAltimetry API Endpoints are available in the following weblink: https://openaltimetry.earthdatacloud.nasa.gov/data/openapi/swagger-ui/index.html
* I updated the *getTracks()* function
* I fixed a bug in the *vsi_time_specific_orbits_wkt()* function
* I modified the "beamName" argument in both the "get_level3a_data()" and "get_atlas_data()" functions to get as input either NULL (data will be returned for all beams) or a character vector of one or more beams. The previous version of the function could take either NULL or a single beam name.
* I updated the  "photonConfidence" which now can take a character vector as input. The available options are 'na', 'noise', 'buffer', 'low', 'medium', 'high'
* I added the *download_file()" function so that the user can adjust the "timeout" parameter using the "options()" settings. The default is now 120 compared to 60 initially.


## IceSat2R 1.0.4

* I fixed a typo error in the documentation of the *get_level3a_data()* function
* I've included the boolean parameter *'download_zip'* to the *'vsi_time_specific_orbits_wkt()'* function. It might be useful if the previous default parameter ( *'download_zip = FALSE'* ) gives a warning or error.
* I've modified an exception in the *'vsi_kml_from_zip()'* function in case the *'sf::gdal_utils(util = 'info')'* function returns an empty character vector ( besides the existing one *'(info_url == "")'* ). I also converted a message to a warning.
* I've modified the *'vsi_time_specific_orbits_wkt()'* function because it gave an error for 'RGT_cycle_4'
* I've updated the documentation of the *'vsi_time_specific_orbits_wkt'* function


## IceSat2R 1.0.3

* I fixed an error related to the *latest_orbits()* function


## IceSat2R 1.0.2

* I've added the *switch_full()* and *switch_abb()* internal functions which allow to process the dates of the downloaded files without taking into account the user's system locale (see issue: https://github.com/mlampros/IceSat2R/issues/3)
* I've updated the documentation by adding more details to each exported function
* I've added more tests that can be executed using *testthat::test_local()* or *testthat::test_file()* (see the section [R package tests](https://github.com/mlampros/IceSat2R#r-package-tests) of the README.md file)
* I added the *ne_10m_glaciated_areas.rda* and *RGT_cycle_14.rda* files in the *data* directory and I removed the *ne_10m_glaciated_areas.RDS* and *RGT_cycle_14.RDS* files from the *inst* directory
* I've added the *verify_RGTs()* function
* I modified / updated the Vignettes, Example sections and the JOSS paper taking into account all previously mentioned changes
* I updated the README.md file
* I modified all files of the R package to make the ICESat-2 naming consistent (see issue https://github.com/mlampros/IceSat2R/issues/8)
* I've modified the "IceSat-2_Atlas_products.Rmd" Vignette files (.html and .pdf versions) to fix an issue related to the "vertical" Coordinate Reference System (CRS) of the Copernicus DEM (see issue: https://github.com/mlampros/IceSat2R/issues/7)
* I fixed a bug in the *time_specific_orbits()* function, as there was a case where only 'RGT' and 'Date_time' was returned, so I had to include the other 2 columns ('DOY', 'Cycle') by using NA's
* I modified and added a test case for the *time_specific_orbits()* function to catch an error case related to [issue 9](https://github.com/mlampros/IceSat2R/issues/9)


## IceSat2R 1.0.1

* I've added the *'IceSat2R.R'* file with required dependencies
* I've added an exception in the *time_specific_orbits()* function to remove internally processed sub-lists that do not have a 'description' (I expect that all '.kml' file sublists are of type 'POINT')
* I've fixed a few minor issues in the vignettes
* I've added the *'available_nominal_orbits()'*, *'vsi_kml_from_zip()'*, *'vsi_nominal_orbits_wkt()'*, *'vsi_time_specific_orbits_wkt()'* and *'latest_orbits()'* functions
* I've added the *'IceSat-2_Virtual_File_System_Orbits_PDF.Rmd'* third vignette 


## IceSat2R 1.0.0
