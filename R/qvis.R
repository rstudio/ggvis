#' Quick vis: a succint way of generating simple ggvis plot specifications.
#'
#' \code{qvis} is a succinct way of creating simple plots. It achieves this
#' succinctness by being a little bit magical and trying to guess what you
#' mean. It is most suitable for creating basic data graphics like scatterplots
#' and histograms. As you start creating more complicated plots, you'll
#' want to use \code{\link{ggvis}} directly to get more control and
#' avoid ambiguity.
#'
#' @section Compared to \code{qplot}:
#'
#' \code{qvis} is somewhat less powerful than \code{qplot}. \code{qvis}
#' contains the most commonly used arguments from \code{qplot}, but it
#' drops the many extra arguments that are easy to replicate by adding
#' the right component on to the plot.
#'
#' @param data The data set where
#' @param ... Properties and layer arguments.
#'
#'   If the name of the argument matches the name of a known property, it will
#'   be added to the plot properties. Otherwise it will be passed as an
#'   argument to every layer. If you want to pass different arguments to
#'   different layers, you'll need to add layers on one at a time.
#'
#'   The first two unnamed components are taken to be \code{x} and \code{y}.
#'   Any additional unnamed components will raise an error.
#' @param layers A character vector listing the names of layers to
#'   display on the plot. You can use either the full name of the layer
#'   (e.g. "layer_smooth"), or just the final part (e.g. "smooth").
#'
#'   If \code{layers} is not supplied, it defaults to "guess". See
#'   \code{\link{layer_guess}} for details.
#' @export
#' @examples
#' # A basic scatterplot
#' mtcars %>% qvis(~mpg, ~wt)
#' mtcars %>% qvis(~mpg, ~wt, fill = ~cyl)
#' mtcars %>% qvis(~mpg, ~wt, fill := "red")
#'
#' # Basic histogram
#' mtcars %>% qvis(~mpg)
#' mtcars %>% qvis(~mpg, binwidth = 2)
#'
#' # Scatterplot + smoother
#' mtcars %>% qvis(~mpg, ~wt, layers = c("points", "smooths"))
#'
#' # It's not currently possible to create a plot of variables
#' # stored only in the local environment
#' x <- runif(10)
#' y <- runif(10)
#' \dontrun{environment() %>% qvis(~x, ~y)}
qvis <- function(data, ..., layers = "guess") {
  args <- dots(...)
  props_args <- props_default_names(args[is_props(args)])
  layer_args <- args[!is_props(args)]

  props <- props(.props = props_args)
  vis <- register_props(ggvis(data), props)

  for (layer in layers) {
    vis <- add_layer(vis, layer, layer_args)
  }

  vis
}

# TODO: make sure this uses the right scoping so that it will find layers
# defined in other packages and in the global environment.
add_layer <- function(vis, name, args = list()) {
  fname <- paste0("layer_", name)

  if (!exists(fname, mode = "function")) {
    stop("Couldn't find layer called ", fname, call. = FALSE)
  }

  layer_f <- get(fname, mode = "function")
  do_call(layer_f, quote(vis), .args = args)
}

props_default_names <- function(args) {
  new_names <- names2(args)

  missing <- new_names == "" & !vapply(args, uses_colon_equals, logical(1))
  n_missing <- sum(missing)

  if (n_missing == 0) return(args)

  if (n_missing == 1) {
    new_names[missing] <- "x"
  } else if (n_missing == 2) {
    new_names[missing] <- c("x", "y")
  } else {
    stop("Only at most two properties can be unnamed", call. = FALSE)
  }

  setNames(args, new_names)
}

# A quoted call is a prop if it's unnamed, or named and in the list of known
# props, or if it's a call to :=
is_props <- function(xs) {
  unnamed <- names2(xs) == ""
  prop_name <- names2(xs) %in% known_props
  uses_colon <- vapply(xs, uses_colon_equals, logical(1))

  unnamed | prop_name | uses_colon
}

# valid_props <- apropos("valid_mark_properties.mark_")
# dput(sort(unique(unlist(lapply(valid_props, function(f) match.fun(f)())))))
known_props <- c("align", "angle", "baseline", "dx", "dy", "endAngle",
  "fill", "fillOpacity", "font", "fontSize", "fontStyle", "fontWeight",
  "height", "innerRadius", "interpolate", "key", "opacity", "outerRadius",
  "shape", "size", "startAngle", "stroke", "strokeOpacity", "strokeWidth",
  "tension", "text", "url", "width", "x", "x2", "y", "y2")
