#' An interactive input bound to resize events.
#'
#' @param vis Visualisation to listen to.
#' @param on_resize Callback function with arguments:
#'   \describe{
#'    \item{width,height}{Width and height in pixels}
#'    \item{padding}{A named list of four components giving the padding in
#'      each direction}
#'    \item{session}{The session, used to communicate with the browser}
#'   }
#' @export
#' @examples
#' # This example just prints out the current dimensions to the console
#' mtcars %>% ggvis(~mpg, ~wt) %>%
#'   layer_points() %>%
#'   handle_resize(function(width, height, ...) cat(width, "x", height, "\n"))
#'
#' # Not very useful example
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_text(text := plot_width())
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_text(text := plot_height())
handle_resize <- function(vis, on_resize) {
  check_callback(on_resize, c("width", "height", "padding", "session"))

  connect <- function(session, plot_id) {
    id <- paste0(plot_id, "_resize")
    shiny::observe({
      value <- session$input[[id]]
      on_resize(width = value$width, height = value$height,
        padding = value$padding, session = value$session)
    })
  }

  broker <- create_broker(
    reactive(NULL),
    connect = connect,
    spec = list(type = "resize")
  )
  register_reactive(vis, broker)
}


#' @export
#' @rdname handle_resize
plot_width <- function(vis) {
  vals <- shiny::reactiveValues()
  vals$x <- 100

  connect <- function(session, plot_id) {
    id <- paste0(plot_id, "_resize")
    shiny::observe({
      value <- session$input[[id]]$width
      if (is.null(value)) return()
      vals$x <- value
    })
  }

  create_broker(
    reactive(vals$x),
    connect = connect,
    spec = list(type = "resize")
  )
}

#' @export
#' @rdname handle_resize
plot_height <- function(vis) {
  vals <- shiny::reactiveValues()
  vals$x <- 100

  connect <- function(session, plot_id) {
    id <- paste0(plot_id, "_resize")
    shiny::observe({
      value <- session$input[[id]]$height
      if (is.null(value)) return()
      vals$x <- value
    })
  }

  create_broker(
    reactive(vals$x),
    connect = connect,
    spec = list(type = "resize")
  )
}

