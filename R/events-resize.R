#' @include events.R
NULL

#' Event broker for resize events.
#'
#' The resize event broker is useful if you want your shiny app to respond
#' to resize events.
#'
#' @export
#' @importFrom methods setRefClass
Resize <- setRefClass("Resize", contains = "EventBroker",
  methods = list(
    resize = function() {
      "A reactive value changes when the plot is resized.
      Returns a list containing:
        * plot_id: The ID of the ggvis plot.
        * width: Width of the plot in pixels.
        * height: Height of the plot in pixels.
        * padding: A list containing `left`, `right`, `top`, `bottom`, which is
          the padding in pixels for each side. The padding is inside of the
          width and height values.
        "

      listen_for("resize")
    }
  )
)

#' An interactive input bound to resize events.
#'
#' @param f A function which is called each time the plot area is resized.
#'
#' @export
#' @examples
#' \dontrun{
#' # This example just prints out the current dimensions to the console
#' print_info <- function(x) {
#'   str(x)
#' }
#'
#' qvis(mtcars, ~mpg, ~wt) + resize(print_info)
#' }
resize <- function(f) {
  stopifnot(is.function(f))
  handler("resize", "resize", list(f = f))
}

#' @export
as.reactive.resize <- function(x, session = NULL, ...) {
  h <- Resize(session, id = x$id)

  obs <- observe({
    r <- h$resize()
    if (is.null(r)) return()

    x$control_args$f(r)
  })

  session$onSessionEnded(function() {
    obs$suspend()
  })

  reactive({ NULL })
}
