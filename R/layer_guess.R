#' Guess the right type of layer based on current properties.
#'
#' \code{layer_guess} provides the magic behind the default behaviour of
#' \code{\link{ggvis}}.
#'
#' @section Defaults:
#'
#' \itemize{
#'   \item Continuous x, \code{\link{layer_histograms}}
#'   \item Categorical x, \code{\link{layer_bars}}
#'   \item Continuous x and y, \code{\link{layer_points}}
#' }
#'
#' @param vis The visualisation to add the new layer to.
#' @param ... Other arguments passed on individual layers.
#' @export
#' @examples
#' # A scatterplot:
#' mtcars %>% ggvis(~mpg, ~wt)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_guess()
#'
#' # A histogram:
#' mtcars %>% ggvis(~mpg)
#' mtcars %>% ggvis(~mpg) %>% layer_guess()
layer_guess <- function(vis, ...) {
  types <- lapply(vis$cur_props, function(x) {
    shiny::isolate(prop_type(x, data = cur_data(vis)))
  })

  types <- types[grepl("\\.update$", names(types))]
  names(types) <- sub("\\.update$", "", names(types))

  layer <- closest(unlist(types), templates)
  message("Guessing layer_", layer, "()")

  f <- match.fun(paste0("layer_", layer))
  f(vis, ...)
}

template <- function(layer, x = NA, y = NA, ...) {
  desc <- c(x = x, y = y, ...)
  stopifnot(is.character(desc))
  desc <- desc[!is.na(desc)]

  structure(list(layer = layer, desc = desc), class = "template")
}

#' @export
print.template <- function(x, ...) {
  cat("<template> ",
    paste0(names(x$desc), " = ", x$desc, collapse = ", "),
    " -> ",
    x$layer,
    "\n",
    sep = "")
}

templates <- list(
  template("bars",       "nominal"),
#   template("boxplots",   "nominal", "numeric"),
  template("bars",       "nominal", "nominal"),

  template("histograms", "numeric"),
  template("freqpolys",  "numeric", stroke = "nominal"),
#   template("boxplots",   "numeric", "nominal"),

  template("points",     "numeric", "numeric"),
  template("lines",      "datetime", "numeric"),
  template("points",     "datetime", "datetime")
)

closest <- function(data, templates) {
  ds <- vapply(templates, distance_n, data = data, FUN.VALUE = numeric(1))
  if (!any(is.finite(ds))) {
    stop("Don't now how to guess a layer for this type of data", call. = FALSE)
  }

  templates[[which.min(ds)]]$layer
}

distance_n <- function(data, template) {
  all <- union(names(data), names(template$desc))

  ds <- unlist(Map(distance_1, data[all], template$desc[all]))
  sum(ds)
}

distance_1 <- function(data, template) {
  if (is.na(data) && is.na(template)) return(0) # Both missing, so can match
  if (is.na(data) && !is.na(template)) return(Inf) # If in template, must be in data
  if (!is.na(data) && is.na(template)) return(0.01)   # If in data, but not template, minor penalty

  n_miss <- is.na(data) + is.na(template)
  if (n_miss == 1) return(Inf) # One missing, so can't match
  if (n_miss == 2) return(0)

  switch(data,
    numeric   = switch(template, numeric = 0, Inf),
    datetime  = switch(template, numeric = 1, datetime = 0, Inf),
    nominal   = switch(template, nominal = 0, Inf),
    ordinal   = switch(template, nominal = 1, ordinal = 0, Inf),
    Inf
  )
}
