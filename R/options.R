#' Set options for a ggvis plot
#'
#' @param vis Visualisation to modify
#' @param width,height Width and height of plot, in pixels. Default is 600x400.
#'   \code{width} or \code{height} can also be \code{"auto"}, in which case the
#'   plot will size to fit in the containing div. This is useful only in a Shiny
#'   app or custom HTML output. Note that \code{height="auto"} should only be
#'   used when the plot is placed within a div that has a fixed height; if not,
#'   automatic height will not work, due to the way that web browsers do
#'   vertical layout.
#' @param keep_aspect Should the aspect ratio be preserved? The default value is
#'   \code{FALSE}, or the value of \code{getOption("ggvis.keep_aspect")}, if it
#'   is set.
#' @param resizable If TRUE, allow the user to resize the plot. The default
#'   value is \code{TRUE}, or the value of \code{getOption("ggvis.resizable")},
#'   if it is set. Not compatible when \code{width} or \code{height} is
#'   \code{"auto"}.
#' @param padding A padding object specifying padding on the top, right, left,
#'   and bottom. See \code{\link{padding}}.
#' @param duration Duration of transitions, in milliseconds.
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"} or
#'   \code{"svg"} (the default).
#' @param hover_duration The amount of time for hover transitions, in
#'   milliseconds.
#'
#' @seealso \code{\link{getOption}} and \code{\link{options}}, for getting and
#'   setting global options.
#' @seealso \code{\link{default_options}} to see the default options.
#' @examples
#' mtcars %>%
#'   ggvis(~wt, ~mpg) %>%
#'   layer_points() %>%
#'   set_options(width = 300, height = 200, padding = padding(10, 10, 10, 10))
#'
#' # Display the default options
#' str(default_options())
#'
#' @export
set_options <- function(vis, width = NULL, height = NULL, keep_aspect = NULL,
                 resizable = NULL, padding = NULL, duration = NULL,
                 renderer = NULL, hover_duration = NULL) {

  options <- compact(list(
    width = shiny::validateCssUnit(width),
    height = shiny::validateCssUnit(height),
    keep_aspect = keep_aspect,
    resizable = resizable,
    padding = padding,
    duration = duration,
    renderer = renderer,
    hover_duration = hover_duration
  ))

  add_options(vis, options)
}

#' Default options
#'
#' This returns an object containing the default options for ggvis.
#'
#' @export
#' @keywords internal
default_options <- function() {
  list(
    width = 600,
    height = 400,
    keep_aspect = getOption("ggvis.keep_aspect", FALSE),
    resizable = getOption("ggvis.resizable", TRUE),
    padding = padding(),
    duration = 250,
    renderer = getOption("ggvis.renderer", "svg"),
    hover_duration = 0
  )
}

# Given a ggvis_opts object, merge it into default options
add_default_options <- function(vis) {
  add_options(vis, default_options(), replace = FALSE)
}

#' Define padding.
#'
#' @param top,right,bottom,left Amount of padding on each border. Can either
#'   be a single number, "auto", or "strict"
#' @export
#' @examples
#' p <- mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()
#' p %>% set_options(padding = padding())
#' p %>% set_options(padding = padding(10, 10, 10, 10))
padding <- function(top = NULL, right = NULL, bottom = NULL, left = NULL) {
  assert_that(is_padding(top), is_padding(right), is_padding(bottom),
    is_padding(left))

  structure(
    compact(list(
      top = top,
      right = right,
      bottom = bottom,
      left = left)
    ),
    class = "padding"
  )
}

#' @export
as.vega.padding <- function(x) {
  if (length(x) == 0) {
    NULL
  } else {
    unclass(x)
  }
}

is_padding <- function(x) {
  if (is.null(x)) return(TRUE)

  length(x) == 1 && (is.numeric(x) || (x %in% c("auto", "strict")))
}
on_failure(is_padding) <- function(call, env) {
  paste0(deparse2(call$x), " is not a single number, 'auto', or 'strict'.")
}
