library(tourr)
library(gigvis)
library(shiny)

aps <- 2
fps <- 30

mat <- rescale(as.matrix(flea[1:6]))
tour <- new_tour(mat, grand_tour(), NULL)
start <- tour(0)

proj_data <- reactive({
  invalidateLater(1000 / fps, NULL);
  step <- tour(aps / fps)
  data.frame(center(mat %*% step$proj), species = flea$species)
})

gigvis(proj_data, props(x ~ X1, y ~ X2, fill ~ species), 
  mark_symbol(), 
  dscale_x_numeric(domain = c(-1, 1)),
  dscale_y_numeric(domain = c(-1, 1))
)