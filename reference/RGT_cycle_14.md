# Reference Ground Tracks (RGTs) for IceSat-2 Cycle 14

The data includes the following columns: "longitude", "latitude",
"day_of_year", "Date", "hour", "minute", "second" and "RGT". The "RGT"
column consists of 1387 Reference Ground Tracks (RGTs) for the IceSat-2
Cycle 14 (from 'December 22, 2021' to 'March 23, 2022')

## Usage

``` r
data(RGT_cycle_14)
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 131765
rows and 8 columns.

## Details

The following code snippet shows how to come to the "RGT_cycle_14" data.
The same can be done with any of the available RGT Cycles. For the
following code I utilized 8 threads to speed up the pre-processing of
the downloaded .kml files (the code takes approximately 15 minutes on my
Linux Personal Computer),

[`require(IceSat2R)`](https://github.com/mlampros/IceSat2R)  
[`require(magrittr)`](https://magrittr.tidyverse.org)  
[`require(sf)`](https://r-spatial.github.io/sf/)  

`avail_cycles = available_RGTs(only_cycle_names = TRUE)`  
`avail_cycles`  

`idx_cycle = 14`  

`choose_cycle = avail_cycles[idx_cycle]`  
`choose_cycle`  

`res_rgt_many = time_specific_orbits(RGT_cycle = choose_cycle, download_method = 'curl', threads = parallel::detectCores(), verbose = TRUE)`  

`RGT_cycle_14 = sf::st_coordinates(res_rgt_many)`  
`colnames(RGT_cycle_14) = c('longitude', 'latitude')`  
`RGT_cycle_14 = data.table::data.table(RGT_cycle_14)`  
`RGT_cycle_14$day_of_year = res_rgt_many$day_of_year`  
`RGT_cycle_14$Date = as.Date(res_rgt_many$Date_time)`  
`RGT_cycle_14$hour = lubridate::hour(res_rgt_many$Date_time)`  
`RGT_cycle_14$minute = lubridate::minute(res_rgt_many$Date_time)`  
`RGT_cycle_14$second = lubridate::second(res_rgt_many$Date_time)`  
`RGT_cycle_14$RGT = res_rgt_many$RGT`  
`RGT_cycle_14`  

## References

https://icesat-2.gsfc.nasa.gov/science/specs

## Examples

``` r
require(IceSat2R)
require(data.table)
#> Loading required package: data.table

data(RGT_cycle_14)
```
