FROM rocker/geospatial:latest
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y pandoc \
    pandoc-citeproc
RUN R -e "install.packages('flexdashboard')"
RUN R -e "install.packages(c('plotly', 'countrycode'))"
RUN R -e "install.packages(c('GGally', 'maps', 'geosphere', 'mapproj', 'sp', 'sna'))"
RUN R -e "install.packages(c('cowplot', 'grid', 'gridExtra', 'openxlsx', 'stringi'))"
RUN R -e "install.packages(c('reshape2', 'rnaturalearth', 'lcowplot'))"
RUN R -e "install.packages(c('tmap', 'lubridate', 'terra', 'spData', 'spDataLarge', 'rnaturalearthdata'))"
RUN R -e "install.packages(c('highcharter', 'mapproj', 'gtable', 'cowplot', 'ggpubr'))"


