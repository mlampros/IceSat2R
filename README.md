
[![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mlampros/IceSat2R/master?urlpath=rstudio)
[![tic](https://github.com/mlampros/IceSat2R/workflows/tic/badge.svg?branch=master)](https://github.com/mlampros/IceSat2R/actions)
[![docs: passing](https://img.shields.io/badge/docs-passing-success.svg)](https://mlampros.github.io/IceSat2R/reference/index.html)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/IceSat2R)](http://cran.r-project.org/package=IceSat2R)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/IceSat2R?color=blue)](http://www.r-pkg.org/pkg/IceSat2R)
[![](https://img.shields.io/docker/automated/mlampros/icesat2r.svg)](https://hub.docker.com/r/mlampros/icesat2r)
[![Dependencies](https://tinyverse.netlify.com/badge/IceSat2R)](https://cran.r-project.org/package=IceSat2R)
[![codecov.io](https://codecov.io/github/mlampros/IceSat2R/coverage.svg?branch=master)](https://codecov.io/github/mlampros/IceSat2R?branch=master)


<img align="right" width="200" src="man/figures/hex_sticker.png"></img>

## IceSat2R

<br>

Programmatic connection to the [OpenAltimetry API](https://openaltimetry.org/data/swagger-ui/) to download and process the following *ICESat-2 Altimeter Data*,

* *'ATL03'* (Global Geolocated Photon Data)
* *'ATL06'* (Land Ice Height)
* *'ATL07'* (Sea Ice Height)
* *'ATL08'* (Land and Vegetation Height)
* *'ATL10'* (Sea Ice Freeboard)
* *'ATL12'* (Ocean Surface Height) 
* *'ATL13'* (Inland Water Surface Height) 

The user has the option to download the data by selecting a bounding box from a 1- or 5-degree grid globally utilizing a shiny application. 

The ICESat-2 mission collects altimetry data of the Earth's surface. The sole instrument on ICESat-2 is the Advanced Topographic Laser Altimeter System (ATLAS) instrument that measures ice sheet elevation change and sea ice thickness, while also generating an estimate of global vegetation biomass. 

ICESat-2 continues the important observations of 

* *ice-sheet elevation change*
* *sea-ice freeboard*, and 
* *vegetation canopy height* 

begun by ICESat in 2003.

<br>

#### **System Requirements**

The usage of the *IceSat2R* package requires a geospatial setup as specified in the [sf](https://github.com/r-spatial/sf#installing) or [terra](https://github.com/rspatial/terra#from-source-code) README.md files.

<br>

### Shiny application to select an area of interest (AOI) from a 1- or 5-degree global grid

<br>

The [OpenAltimetry API](https://openaltimetry.org/data/swagger-ui/) restricts the requests to a *1x1* or *5x5* degree spatial bounding box, unless the *"sampling"* parameter is set to *TRUE*. The shiny application of the *IceSat2R* package allows the user to create a spatial grid of an AOI, preferably a 1- or 5-degree grid so that the selection can be within limits. An alternative would be to create a grid of smaller grid cells than required (for instance a 4-degree grid) and then to select multiple grid cells,

<br>

<img src="man/figures/shiny_app_grid.gif" ></img>

<br>

### Example Use Case: 3-Dimensional Line Plot by combining IceSat-2 and Copernicus DEM (Digital Elevation Model) Data

<br>

The following 3-dimensional interactive line plot (which appears in the ['IceSat-2_Atlas_products' Vignette](https://mlampros.github.io/IceSat2R/articles/IceSat-2_Atlas_products_HTML.html)) shows,

* in *blue* color the *elevation* based on the *DEM* compared to the two ICESat-2 beams ('gt1r' and 'gt2l'), as these are separated by a 3-km distance
* in *orange* color the land-ice-height measurements of the *summer* period (separately for 'gt1r' and 'gt2l')
* in *green* color the land-ice-height measurements of the *winter* period (separately for 'gt1r' and 'gt2l')

<br>

<img src="man/figures/3_dim_plot.gif" ></img>

<br>

#### **Binder**

The user of the **IceSat2R** R package can reproduce the examples of the documentation using the available [binder Rstudio image](https://mybinder.org/v2/gh/mlampros/IceSat2R/master?urlpath=rstudio). Once launched the cloud instance will take a few minutes to be ready. You can read more [about binder on the web](https://mybinder.org/). In short, binder allows to make "your code immediately reproducible by anyone, anywhere".

<br>

#### **Docker Image**

<br>

**Docker images** of the *IceSat2R* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/icesat2r) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/icesat2r:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/icesat2r:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/icesat2r:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [www.rocker-project.org](https://www.rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

To install the package from CRAN use, 

```R
install.packages("IceSat2R")

```
<br>

and to download the latest version of the package from Github,

```R
remotes::install_github('mlampros/IceSat2R')

```

<br>

### Citation:

<br>

If you use the code of this repository in your paper or research please cite both **IceSat2R** and the **original articles** (see CITATION) `https://CRAN.R-project.org/package=IceSat2R`:

<br>

```R
@Manual{,
  title = {{IceSat2R}: ICESat-2 Altimeter Data using R},
  author = {Lampros Mouselimis},
  year = {2022},
  note = {R package version 1.0.0},
  url = {https://CRAN.R-project.org/package=IceSat2R},
}
```

<br>

## Code of Conduct
  
  Please note that the IceSat2R project is released with a [Contributor Code of Conduct](https://github.com/mlampros/IceSat2R/blob/master/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.


## Acknowledgment

This project received financial [support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://www.r-consortium.org/wp-content/uploads/sites/13/2016/09/RConsortium_Horizontal_Pantone.png" width="300">
</a>

<br>
