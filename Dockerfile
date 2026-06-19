FROM rocker/shiny:4
LABEL org.opencontainers.image.authors="www.marine.ie"

RUN sudo apt-get update && apt-get upgrade -y && apt-get install -y build-essential \
&& apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN sudo apt-get update && \
  apt-get install -y cmake make libuv1-dev pandoc git && \
  rm -rf /var/lib/apt/lists/* && rm -rf /tmp/downloaded_packages/ /tmp/*.rds 

# install additional packages
RUN R -q -e 'install.packages(c("dplyr","geosphere","bslib","DT","remotes","icesDatras"), repos="https://cran.rstudio.com/")'
RUN R -q -e 'remotes::install_github("Franvgls/NeAtlIBTS64")'

RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
