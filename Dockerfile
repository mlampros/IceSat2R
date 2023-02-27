FROM mlampros/icesat2r:binder

#......................................................................................
# References: https://hub.docker.com/r/rocker/binder
#             https://github.com/yuvipanda/rstudio-binder-template/blob/main/Dockerfile
#......................................................................................

LABEL maintainer='Lampros Mouselimis'

## Declares build arguments
ARG NB_USER
ARG NB_UID

COPY --chown=${NB_USER} . ${HOME}

ENV DEBIAN_FRONTEND=noninteractive
USER root
RUN echo "Checking for 'apt.txt'..." \
        ; if test -f "apt.txt" ; then \
        apt-get update --fix-missing > /dev/null\
        && xargs -a apt.txt apt-get install --yes \
        && apt-get clean > /dev/null \
        && rm -rf /var/lib/apt/lists/* \
        ; fi

USER ${NB_USER}

## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi
