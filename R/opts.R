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
#'
#' @seealso \code{link{getOption}} and \code{link{options}}, for getting and
#'   setting global options.
#' @examples
#' ggvis(mtcars, props(x = ~wt, y = ~mpg), mark_symbol(),
#'   opts(width = 300, height = 200, padding = padding(10, 10, 10, 10)))
#'
#' @export
opts <- function(width = NULL, height = NULL, keep_aspect = NULL,
                 resizable = NULL, padding = NULL, duration = NULL,
                 renderer = NULL) {
  structure(
    compact(list(
      width = width,
      height = height,
      keep_aspect = keep_aspect,
      resizable = resizable,
      padding = padding,
      duration = duration,
      renderer = renderer
    )),
    class = "ggvis_opts"
  )
}

#' @S3method as.vega ggvis_opts
as.vega.ggvis_opts <- function(x) x

# Default options
default_opts <- function() {
  structure(
    list(
      width = 600,
      height = 400,
      keep_aspect = getOption("ggvis.keep_aspect", FALSE),
      resizable = getOption("ggvis.resizable", TRUE),
      padding = padding(),
      duration = 250,
      renderer = getOption("ggvis.renderer", "canvas")
    ),
    class = "ggvis_opts"
  )
}

# Given a ggvis_opts object, merge it into default options
add_default_opts <- function(x) {
  structure(modifyList(default_opts(), x), class = "ggvis_opts")
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

#' @S3method as.vega padding
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
