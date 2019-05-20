library(geomdesign)
library(readr)
 design <- read_csv("data/canola-is-the-worst.csv") %>% 
   select(ID, ROW, RANGE, REP, uuid = id) %>% 
   mutate(ROW = as.integer(ROW),
          RANGE = as.integer(RANGE))

canola_poly <-  sf::st_as_sf(geojsonio::geojson_read("data/canola_polygon.json", what = "sp"))
 

canola_json <- geomdesign::design_sf(design, canola_poly, RANGE, ROW)
geojsonio::geojson_write(canola_json, file = "data/canola-uuid.json")  

ex_json <- make_geojson(design2, poly, ROW, RANGE)
geojson_write(ex_json, file = "data/example.json")

inherits(design[["RANGE"]], "integer")