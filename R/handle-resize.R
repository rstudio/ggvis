#' An interactive input bound to resize events.
#'
#' @param vis Visualisation to listen to.
#' @param on_resize Callback function called with arguments \code{value} and
#'   \code{session} every time plot size changes.
#' @export
#' @examples
#' # This example just prints out the current dimensions to the console
#' mtcars %>% ggvis(~mpg, ~wt) %>%
#'   layer_points() %>%
#'   handle_resize(function(value, ...) str(value))
handle_resize <- function(vis, on_resize = NULL) {
  check_callback(on_resize, c("value", "session"))

  connect <- function(session, plot_id) {
    setup_callback(on_resize, paste0(plot_id, "_resize"), session)
  }

  broker <- create_broker(
    reactive(NULL),
    connect = connect,
    spec = list(type = "resize")
  )
  register_reactive(vis, broker)

}
