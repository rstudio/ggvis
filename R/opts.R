#' Set options for a ggvis plot
#'
#' @param width,height Width and height of plot, in pixels. Default is 600x400.
#' @param keep_aspect Should the aspect ratio be preserved? The default value
#'   is \code{FALSE}, or the value of \code{getOption("ggvis.keep_aspect")}, if
#'   it is set.
#' @param resizable If TRUE, allow the user to resize the plot.
#'   The default value is \code{TRUE}, or the value of
#'   \code{getOption("ggvis.resizable")}, if it is set.
#' @param padding A padding object specifying padding on the top, right, left,
#'   and bottom. See \code{\link{padding}}.
#' @param duration Duration of transitions, in milliseconds.
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"}
#'   (the default) or \code{"svg"}.
#' @param hover_duration The amount of time for hover transitions, in
#'   milliseconds.
#'
#' @seealso \code{link{getOption}} and \code{link{options}}, for getting and
#'   setting global options.
#' @seealso \code{link{default_opts}} to see the default options.
#' @examples
#' qvis(mtcars, ~wt, ~mpg) +
#'   opts(width = 300, height = 200, padding = padding(10, 10, 10, 10))
#'
#' # Display the default options
#' str(default_opts())
#'
#' @export
opts <- function(width = NULL, height = NULL, keep_aspect = NULL,
                 resizable = NULL, padding = NULL, duration = NULL,
                 renderer = NULL, hover_duration = NULL) {

  structure(
    compact(list(
      width = width,
      height = height,
      keep_aspect = keep_aspect,
      resizable = resizable,
      padding = padding,
      duration = duration,
      renderer = renderer,
      hover_duration = hover_duration
    )),
    class = "ggvis_opts"
  )
}

#' @export
as.vega.ggvis_opts <- function(x) x


#' @rdname opts
#' @export
#' @param x an object to test for opts-ness.
is.ggvis_opts <- function(x) inherits(x, "ggvis_opts")

#' Default options
#'
#' This returns an object containing the default options for ggvis.
#'
#' @export
default_opts <- function() {
  structure(
    list(
      width = 600,
      height = 400,
      keep_aspect = getOption("ggvis.keep_aspect", FALSE),
      resizable = getOption("ggvis.resizable", TRUE),
      padding = padding(),
      duration = 250,
      renderer = getOption("ggvis.renderer", "svg"),
      hover_duration = 0
    ),
    class = "ggvis_opts"
  )
}

# Given a ggvis_opts object, merge it into default options
add_default_opts <- function(x) merge_opts(default_opts(), x)

# Get options from knitr, if present
knitr_opts <- function() {
  if (!is_installed("knitr")) {
    stop("knitr must be installed to call this function.")
  }
  get_opt <- knitr::opts_current$get

  width = get_opt('fig.width') * get_opt('dpi')
  height = get_opt('fig.height') * get_opt('dpi')

  retina = get_opt('fig.retina')
  if (!is.null(retina)) {
    width = width / retina
    height = height / retina
  }

  opts(width = width, height = height)
}

# Merge two opts() objects
merge_opts <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  stopifnot(is.ggvis_opts(a), is.ggvis_opts(b))

  structure(modifyList(a, b), class = "ggvis_opts")
}

#' Define padding.
#'
#' @param top,right,bottom,left Amount of padding on each border. Can either
#'   be a single number, "auto", or "strict"
#' @export
#' @examples
#' opts(padding = padding())
#' opts(padding = padding(10, 10, 10, 10))
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
