
## IceSat2R 1.0.1

* I've added the *'IceSat2R.R'* file with required dependencies
* I've added an exception in the *time_specific_orbits()* function to remove internally processed sub-lists that do not have a 'description' (I expect that all '.kml' file sublists are of type 'POINT')
* I've fixed a few minor issues in the vignettes
* I've added the *'available_nominal_orbits()'*, *'vsi_kml_from_zip()'*, *'vsi_nominal_orbits_wkt()'*, *'vsi_time_specific_orbits_wkt()'* and *'latest_orbits()'* functions
* I've added the *'IceSat-2_Virtual_File_System_Orbits_PDF.Rmd'* third vignette 


## IceSat2R 1.0.0
