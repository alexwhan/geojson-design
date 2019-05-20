library(kriging)
n <- 100
krig <- kriging(runif(6), runif(6), runif(6), lag = 3)
image(krig)
