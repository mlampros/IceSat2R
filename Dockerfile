FROM rocker/geospatial:latest

LABEL maintainer='Lampros Mouselimis'

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \
 apt install unzip && \
 R -e "install.packages('devtools', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \
 R -e "install.packages(c( 'glue', 'sf', 'lwgeom', 'units', 'data.table', 'httr', 'utils', 'foreach', 'tools', 'doParallel', 'magrittr', 'leaflet', 'leafgl', 'leaflet.extras', 'leafsync', 'miniUI', 'shiny', 'rnaturalearth', 'rmarkdown', 'knitr', 'lubridate', 'DT', 'mapview', 'grDevices', 'stargazer', 'reshape2', 'plotly', 'geodist', 'CopernicusDEM', 'terra', 'testthat', 'remotes' ), repos =  'https://cloud.r-project.org/' )"

RUN R -e "remotes::install_github('mlampros/IceSat2R', upgrade = 'always', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \
 apt-get autoremove -y && \
 apt-get clean

ENV USER rstudio
