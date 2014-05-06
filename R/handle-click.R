#' Handle mouse actions on marks.
#'
#' @param vis Visualisation to listen to.
#' @param on_click,on_mouse_over,on_mouse_out Callback functions called with
#'   arguments \code{value} and \code{session} every time the mouse is clicked,
#'   mouses over an object, or mouses out.
#' @export
#' @examples
#' str_value <- function(value, ...) str(value)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>%
#'   handle_click(str_value)
#' mtcars %>% ggvis(~mpg, ~wt) %>% layer_points() %>%
#'   handle_hover(str_value, str_value)
handle_click <- function(vis, on_click = NULL) {
  check_callback(on_click, c("value", "session"))

  connect <- function(session, plot_id) {
    setup_callback(on_click, paste0(plot_id, "_mouse_click"), session)
  }

  spec <- list(type = "click")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)
  register_reactive(vis, broker)
}

#' @rdname handle_click
#' @export
handle_hover <- function(vis, on_mouse_over = NULL, on_mouse_out = NULL) {
  check_callback(on_mouse_over, c("value", "session"))
  check_callback(on_mouse_out, c("value", "session"))

  connect <- function(session, plot_id) {
    setup_callback(on_mouse_over, paste0(plot_id, "_mouse_over"), session)
    setup_callback(on_mouse_out, paste0(plot_id, "_mouse_out"), session)
  }

  spec <- list(type = "hover")
  broker <- create_broker(reactive(NULL), connect = connect, spec = spec)
  register_reactive(vis, broker)
}

