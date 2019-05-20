FROM rocker/geospatial

RUN install2.r --error \
  settings \
  automap \
  geojsonio
RUN git clone https://bitbucket.csiro.au/scm/sc/rsenaps.git
RUN R -e "install.packages('/rsenaps', repos = NULL, type = 'source')"
