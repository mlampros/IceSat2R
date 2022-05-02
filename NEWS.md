
## IceSat2R 1.0.2

* I've added the *switch_full()* and *switch_abb()* internal functions which allow to process the dates of the downloaded files without taking into account the user's system locale (see issue: https://github.com/mlampros/IceSat2R/issues/3)
* I've updated the documentation by adding more details to each exported function
* I've updated the README.md file
* I've added more tests that can be executed using *testthat::test_local()* or *testthat::test_file()* (see the section *R package tests* of the README.md file)


## IceSat2R 1.0.1

* I've added the *'IceSat2R.R'* file with required dependencies
* I've added an exception in the *time_specific_orbits()* function to remove internally processed sub-lists that do not have a 'description' (I expect that all '.kml' file sublists are of type 'POINT')
* I've fixed a few minor issues in the vignettes
* I've added the *'available_nominal_orbits()'*, *'vsi_kml_from_zip()'*, *'vsi_nominal_orbits_wkt()'*, *'vsi_time_specific_orbits_wkt()'* and *'latest_orbits()'* functions
* I've added the *'IceSat-2_Virtual_File_System_Orbits_PDF.Rmd'* third vignette 


## IceSat2R 1.0.0
