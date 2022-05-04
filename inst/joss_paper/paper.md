---
title: 'ICESat-2 Altimeter Data using R'
tags:
  - R
  - ICESat-2
  - OpenAltimetry
authors:
  - name: Lampros Sp. Mouselimis
    orcid: 0000-0002-8024-1546
    affiliation: 1
affiliations:
 - name: Monopteryx, Rahouli Paramythias, Thesprotia, Greece
   index: 1
citation_author: Lampros Sp. Mouselimis
date: 05 April 2022
year: 2022
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---




# Summary

Lidar technology (light detection and ranging) is integrated into autonomous cars, unmanned aerial vehicles (UAV), airplanes, and satellites and as of October 2021, it represents a growing market. Many satellite missions utilize lidar to observe the surface of the earth and one of these is ICESat-2, which was launched on 15th September 2018 with the primary goal to measure changes in glaciers and ice sheets [@smith2019]. ICESat-2 (the successor of ICESat) consists of 6 beams (3 pairs), where each pair is separated by 3 kilometers and each beam in the pair is also separated by 90 meters as the following image shows [@smith2019],

\begin{figure}[h]
\includegraphics[width=1\linewidth,height=0.3\textheight]{figures/icesat_2_details} \caption{the ICESat-2 ATLAS six-beam pattern}\label{fig:icesat-beams}
\end{figure}

Besides the [Distributed Active Archive Center (DAAC) at NSIDC](https://nsidc.org/daac/) another source to retrieve ICESat-2 data is the [OpenAltimetry platform](https://openaltimetry.org/about.html), which allows users to discover, access, and visualize data from NASA's ICESat and ICESat-2 missions [@khalsa2020]. 

The [IceSat2R](https://cran.r-project.org/web/packages/IceSat2R/index.html) package [@mouselimis2022] includes functionality that

* creates a connection to the [OpenAltimetry Web API](https://openaltimetry.org/data/swagger-ui/)
* allows users to interactively create a global degree-grid, which is required for the OpenAltimetry queries
* makes feasible the download of the required Reference Ground Tracks (RGTs) from the [National Snow & Ice Data Center (NSIDC)](https://icesat-2.gsfc.nasa.gov/science/specs)
* includes three vignettes that explain in detail how users can analyze ICESat-2 data and combine it with other satellite and in-situ sources.


# Statement of need

Online access to data and analysis tools via easy-to-use interfaces can significantly increase data usage across a wide range of users [@khalsa2020]. In the same way, authors of programming packages should incorporate the required functionality so that the biggest possible number of users can access and take advantage of the codebase. An important aspect of the [IceSat2R](https://cran.r-project.org/web/packages/IceSat2R/index.html) package is that it includes the code, documentation, and examples so that users can retrieve, process, and analyze data based on specific workflows. For instance,

- A user can select an *area of interest* (AOI) either programmatically or interactively

- If the *Reference Ground Track* (RGT) is not known, the user has the option to utilize either

    -   one of the **'overall_mission_orbits()'** or **'time_specific_orbits()'** to compute the RGT(s) for a pre-specified global area or for a time period, or
    -   one of the **'vsi_nominal_orbits_wkt()'** or **'vsi_time_specific_orbits_wkt()'** to compute the RGT(s) for a specific AOI

- Once the RGT is computed it can be verified with the **'getTracks()'** function of the [OpenAltimetry Web API](https://openaltimetry.org/data/swagger-ui/)

- Finally the user can utilize one of the **'get_atlas_data()'** or **'get_level3a_data()'** functions to retrieve the data for specific product(s), Date(s) and Beam(s)

This work-flow is illustrated also in the next diagram,

\begin{figure}[h]

{\centering \includegraphics[width=1\linewidth,height=0.4\textheight]{figures/icesat_2_diagram} 

}

\caption{IceSat2R Workflow Diagram}\label{fig:icesat2-diagram}
\end{figure}


# Example use case

The example code of this section explains how an R user can utilize the [IceSat2R](https://cran.r-project.org/web/packages/IceSat2R/index.html) package to,

-   extract the *Reference Ground Tracks (RGT)* of an AOI (*Himalayas mountain range*) using the *GDAL Virtual File System*
-   retrieve data from the *OpenAltimetry API* for a pre-specified bounding box (as described in the previous work-flow)
-   report the number of downloaded and available observations for each specified ICESat-2 product

The following map shows the bounding box areas (small on the left and big on the right) that will be used in the code.

\begin{figure}[h]

{\centering \includegraphics[width=1\linewidth,height=0.15\textheight]{figures/himalayas_aoi} 

}

\caption{Himalayas AOI}\label{fig:himalayas-aoi}
\end{figure}


```r
# required R packages

pkgs = c('IceSat2R', 'magrittr', 'sf', 'mapview', 'leaflet', 
         'glue', 'data.table')
load_pkgs = lapply(pkgs, require, character.only = TRUE)

# load the AOI

himl_pth = system.file('data_files', 'vignette_data', 'himalayas.RDS',
                       package = "IceSat2R")
geoms_himal = readRDS(himl_pth)

sf_wkt = sf::st_geometry(subset(geoms_himal, area_size == 'big'))
centr_wkt = sf::st_coordinates(sf::st_centroid(sf_wkt))
dat_wkt = sf::st_as_text(sf_wkt)

# iterate over all 8 available repeats for the Eastern Hemisphere

lst_out = list()

for (iter in 1:8) {        
  dat_iter = IceSat2R::vsi_nominal_orbits_wkt(orbit_area = 'eastern_hemisphere',
                                              track = 'GT7',
                                              rgt_repeat = iter,
                                              wkt_filter = dat_wkt,
                                              download_method = 'curl',
                                              download_zip = FALSE,
                                              verbose = TRUE)
  lst_out[[iter]] = dat_iter
}

# Extract the unique Reference Ground Tracks (RGTs)

lst_out = unlist(lst_out, recursive = FALSE)
unq_rgts = as.vector(unique(unlist(lapply(lst_out, function(x) x$RGT))))
unq_rgts
# [1] "96"   "157"  "363"  "538"  "599"  "805"  "866"  "1041" "1308" "1247"
```

In this use case, we are interested in ICESat-2 data for a specific time period (from *'2020-01-01'* to *'2021-01-01'* - 1-year's data). We'll make use of the *IceSat2R::vsi_time_specific_orbits_wkt()* function which queries all *15 ICESat-2 RGTs cycles* (as of March 2022) to come to the RGTs intersection for the specified 1-year time interval,


```r
date_start = '2020-01-01'
date_end = '2021-01-01'

orb_cyc_multi = IceSat2R::vsi_time_specific_orbits_wkt(date_from = date_start,
                                                       date_to = date_end,
                                                       RGTs = unq_rgts,
                                                       wkt_filter = dat_wkt,
                                                       verbose = TRUE)
```

The next map shows the 18 different Date-Time matches for our defined 1-year time period based on the output of the *'IceSat2R::vsi_time_specific_orbits_wkt()'* function,


```r
orbit_cy = mapview::mapview(orb_cyc_multi, legend = FALSE)
AOI_wkt = mapview::mapview(sf_wkt, legend = FALSE)

lft = orbit_cy + AOI_wkt

lft@map %>% leaflet::setView(lng = centr_wkt[, 'X'],
                             lat = centr_wkt[, 'Y'],
                             zoom = 7)
```

\begin{figure}[h]

{\centering \includegraphics[width=1\linewidth,height=0.35\textheight]{figures/himalayas_rgts} 

}

\caption{Intersection of the AOI and the ICESat-2 Orbits}\label{fig:himalayas-rgts}
\end{figure}

The output of *'vsi_time_specific_orbits_wkt()'* can be verified with the *OpenAltimetry's 'getTracks()'* function,


```r
bbx_aoi = sf::st_bbox(obj = sf_wkt)
dtbl_rgts = verify_RGTs(nsidc_rgts = orb_cyc_multi, 
                        bbx_aoi = bbx_aoi, 
                        verbose = TRUE)
rows_match = which(dtbl_rgts$RGT_OpenAlt == dtbl_rgts$RGT_NSIDC)
```

We keep the rows that intersect with the output of the *OpenAltimetry getTracks()* function,


```r
inters_opnalt_rgt = orb_cyc_multi[rows_match, , drop = FALSE]
```

We also restrict our initial AOI to a smaller area in the *Himalayas mountain range*,


```r
sf_wkt_init = sf::st_geometry(subset(geoms_himal, area_size == 'small'))
bbx_aoi_init = sf::st_bbox(obj = sf_wkt_init)
```

A potential use case would be to visualize the *Ice*, *Land* and *Canopy* height differences, and the right function for this purpose would be the *IceSat2R::get_level3a_data()* function that takes a *time interval* as input. The corresponding ICESat-2 and OpenAltimetry API products are the **'atl06'** and **'atl08'**. In this example use case we'll restrict to the *'atl06'* product but iterating over multiple Products is also feasible,


```r
RGTs = sort(unique(inters_opnalt_rgt$RGT))
```

We'll iterate over the available tracks to retrieve the altimeter data,


```r
dat_out = logs_out = list()

for (track_i in RGTs) {
  
  cat(paste0(track_i, '.'))
  prod = 'atl06'

  iter_dat = IceSat2R::get_level3a_data(minx = bbx_aoi_init['xmin'],
                                        miny = bbx_aoi_init['ymin'],
                                        maxx = bbx_aoi_init['xmax'],
                                        maxy = bbx_aoi_init['ymax'],
                                        startDate = date_start,
                                        endDate = date_end,
                                        trackId = track_i,
                                        beamName = NULL,
                                        product = prod,
                                        client = 'portal',
                                        outputFormat = 'csv',
                                        verbose = FALSE)
  iter_logs = list(RGT = track_i,
                   Product = prod,
                   N_rows = nrow(iter_dat))
  
  logs_out[[as.character(track_i)]] = data.table::setDT(iter_logs)
  dat_out[[as.character(track_i)]] = iter_dat
}
```

The following table shows the RGTs and the retrieved number of rows for the specified ICESat-2 products.


```r
# output logs

dtbl_logs = data.table::rbindlist(logs_out) %>%
  subset(N_rows > 0)
dtbl_logs = dtbl_logs[order(dtbl_logs$N_rows, decreasing = TRUE), ]
dtbl_logs

#     RGT Product N_rows
# 1:  599   atl06  56531
# 2:   96   atl06  51874
# 3: 1041   atl06  49208
# 4:  538   atl06   1535
```


# Acknowledgements

The development of the [IceSat2R](https://cran.r-project.org/web/packages/IceSat2R/index.html) package was supported by the [R Consortium](https://www.r-consortium.org/projects) (grant code 21-ISC-2-2).


# References
