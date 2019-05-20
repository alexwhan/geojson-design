FROM rocker/geospatial
MAINTAINER "Carl Boettiger" cboettig@ropensci.org

RUN install2.r --error \
  settings \
  automap \
  geojsonio
RUN git clone https://wha022@bitbucket.csiro.au/scm/sc/rsenaps.git
RUN R -e "install.packages('/rsenaps', repos = NULL, type = 'source')"
