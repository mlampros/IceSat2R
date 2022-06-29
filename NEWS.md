
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
