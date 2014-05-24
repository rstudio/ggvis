#' Guess the right type of layer based on current properties.
#'
#' \code{layer_guess} provides the magic behind the default behaviour of
#' \code{\link{qvis}}.
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
#' mtcars %>% qvis(~mpg, ~wt)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_guess()
#'
#' # A histogram:
#' mtcars %>% qvis(~mpg)
#' mtcars %>% ggvis(~mpg) %>% layer_guess()
layer_guess <- function(vis, ...) {
  props <- vis$cur_props
  data <- cur_data(vis)

  if ("y.update" %in% names(props)) {
    layer_points(vis, ...)
  } else {
    if (prop_countable(data, props$x)) {
      layer_bars(vis, ...)
    } else {
      layer_histograms(vis, ...)
    }
  }
}

# c = continuous
#  g = granular
#  d = date/time
#
# n = nominal
#  o = ordinal

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
  template("bars",       "n"),
  template("boxplots",   "n", "c"),
  template("bars",       "n", "n"),

  template("histograms", "c"),
  template("freqpolys",  "c", stroke = "n"),
  template("boxplots",   "c", "n"),

  template("points",     "c", "c"),
  template("points",     "g", "g"),
  template("lines",      "d", "c")
  template("points",     "d", "d")
)

closest <- function(data, templates) {
  ds <- vapply(templates, distance_n, data = data, FUN.VALUE = numeric(1))
  ds <- ds[is.finite(ds)]
  if (length(ds) == 0) {
    stop("No matching templates found", call. = FALSE)
  }

  templates[which.min(ds)]
}

distance_n <- function(data, template) {
  all <- union(names(data), names(template$desc))

  ds <- unlist(Map(distance_1, data[all], template$desc[all]))
  sum(ds)
}

distance_1 <- function(data, template) {
  if (is.na(data) && is.na(template)) return(0) # Both missing, so can match
  if (is.na(data) && !is.na(template)) return(Inf) # If in template, must be in data
  if (!is.na(data) && is.na(template)) return(0)   # If in data, but not template, ignore

  n_miss <- is.na(data) + is.na(template)
  if (n_miss == 1) return(Inf) # One missing, so can't match
  if (n_miss == 2) return(0)

  switch(data,
    c = switch(template, c = 0, Inf),
    d = switch(template, c = 1, d = 0, Inf),
    g = swtich(template, c = 1, g = 0, Inf),
    n = switch(template, n = 0, Inf),
    o = switch(template, n = 1, o = 0, Inf),
    Inf
  )
}
