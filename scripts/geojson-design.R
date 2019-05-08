library(geojsonio)
library(sp)
library(dplyr)
library(purrr)
library(readr)

poly <- geojsonio::geojson_read("data/polygon.json")

furthest <- function(feature, axis = 1, reverse = FALSE) {
  coords <- feature$geometry$coordinates[[1]]
  coords_vec <- unlist(map(coords[1:(length(coords) - 1)], ~ .x[axis]))
  if(!reverse) return(which.min(coords_vec)) else which.max(coords_vec)
}

design <- read_csv("data/canola-design.csv", skip = 1) %>% 
  select(ID, ENTRY, ROW, RANGE, ps_id = `field packet stem id`) %>% 
  mutate(ROW = as.integer(ROW),
         RANGE = as.integer(RANGE))

make_geojson <- function(design, polygon, dim1_var = RUN, dim2_var = RANGE) {
  
  dim1_quo <- enquo(dim1_var)
  dim1_var <- deparse(substitute(dim1_var))
  dim2_quo <- enquo(dim2_var)
  dim2_var <- deparse(substitute(dim2_var))
  #check that dim1_var and dim2_var terms are in the design
  # stopifnot(all(c(dim1_var, dim2_var) %in% names(design)))
  
  #check that dim1_var and dim2_var terms are integers
  stopifnot(inherits(design[[dim1_var]], "integer"),
            inherits(design[[dim2_var]], "integer"))
  
  #check if the dim1_var and dim2_var terms give unique combinations
  stopifnot(nrow(design) == nrow(design[, c(dim1_var, dim2_var)]))
  
  ndim1_var <- max(design[[dim1_var]])
  ndim2_var <- max(design[[dim2_var]])
  
  #find the most extreme south, west, north points to calculate the deltas for dim1 and dim2
  south <- furthest(poly$features[[1]], 1)
  west <- furthest(poly$features[[1]], 2)
  north <- furthest(poly$features[[1]], 1, TRUE)

  coords <- poly$features[[1]]$geometry$coordinates[[1]]
  
  #calculate delta for each dim for x/y
  dim1x <- (coords[[west]][[1]] - coords[[south]][[1]]) / ndim1
  dim2x <- (coords[[north]][[1]] - coords[[west]][[1]]) / ndim2
  dim1y <- (coords[[west]][[2]] - coords[[south]][[2]]) / ndim1
  dim2y <- (coords[[north]][[2]] - coords[[west]][[2]]) / ndim2
  
  #all output coords are based on this point
  minx <- coords[[south]][[1]]
  miny <- coords[[south]][[2]]
  
  #A list used to make polygons
  corners <- list(
    c1 = c(1, 1),
    c2 = c(0, 1),
    c3 = c(0, 0),
    c4 = c(1, 0),
    c5 = c(1, 1)
  )
  # browser()
  design_sf <- design %>% 
    rowwise %>% 
    mutate(poly = list(make_poly(!!dim1_quo, !!dim2_quo, runx, rangex, runy, rangey, coords[[south]][[1]], coords[[south]][[2]], coords = corners))) %>% 
    sf::st_sf()
}

make_points <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = 0, moddim2 = 0) {
  x <- (dim1 - moddim1) * dim1x + (dim2 - moddim2) * dim2x + refx
  y <- (dim1 - moddim1) * dim1y + (dim2 - moddim2) * dim2y + refy
  return(c(x = x, y = y))
}

make_poly <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, coords = NULL) {
  stopifnot(!is.null(coords))
  coords_out <- map(coords, ~ make_points(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = .x[1], moddim2 = .x[2]))
  st_polygon(list(matrix(unlist(coords_out), ncol = 2, byrow = TRUE)))
}

canola_json <- make_geojson(design, poly, ROW, RANGE)
geojson_write(canola_json, file = "data/canola.json")  
