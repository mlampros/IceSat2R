FROM mlampros/icesat2r:rstudiodev

LABEL maintainer='Lampros Mouselimis'

RUN apt-get install -y python3-pip && \
    pip install --no-cache --upgrade pip && \
    pip install --no-cache notebook jupyterlab

USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

USER ${NB_USER}
