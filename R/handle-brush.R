#' Handle brush events on a visualisation.
#'
#' @param vis Visualisation to listen to.
#' @param on_move Callback function called with arguments \code{value} and
#'   \code{session} every time the brush moves. Value is a list of points
#'   under the brush.
#' @param fill Colour of the brush.
#' @export
#' @examples
#' # Display tooltip when objects are brushed
#' mtcars %>% ggvis(x = ~wt, y = ~mpg, size.brush := 400) %>%
#'   layer_points() %>%
#'   handle_brush(function(value, session) {
#'     show_tooltip(session, pagex = value$pagex2 + 5,
#'      pagey = value$pagey1 + 5, html = length(value$items))
#'   })
handle_brush <- function(vis, on_move = NULL, fill = "black") {
  check_callback(on_move, c("value", "session"))

  connect <- function(session, plot_id) {
    setup_callback(on_move, paste0(plot_id, "_brush_move"), session)
  }
  connector_label(connect) <- "brush"

  broker <- create_broker(
    reactive(NULL),
    connect = connect,
    spec = list(type = "brush")
  )
  vis <- register_reactive(vis, broker)

  layer_brush(vis, fill = fill)
}

layer_brush <- function(vis, fill = "black") {
  layer_f(vis, function(v) {
    init <- data.frame(x = 0, y = 0, width = 0, height = 0)
    v <- add_data(v, init, "ggvis_brush")
    emit_rects(v, props(x := ~x, y := ~y,
      width := ~width, height := ~height,
      fill := fill, fillOpacity := 0.2,
      stroke := fill, strokeOpacity := 0.6,
      inherit = FALSE))
  })
}
