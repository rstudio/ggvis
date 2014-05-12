#' Handle brush events on a visualisation.
#'
#' Currently for brush events to be triggered on a visualisation, you must
#' use a \code{.brush} property. This limitation will be lifted in the future.
#'
#' @param vis Visualisation to listen to.
#' @param on_move Callback function with arguments:
#'   \describe{
#'    \item{items}{A data frame containing information about the items
#'      under the plot. An empty data.frame if no points under the brush.}
#'    \item{page_loc}{Location of the brush with repsect to the page}
#'    \item{plot_loc}{Location of the brush with respect to the plot}
#'    \item{session}{The session, used to communicate with the browser}
#'   }
#' @param fill Colour of the brush.
#' @export
#' @examples
#' # Display tooltip when objects are brushed
#' mtcars %>%
#'   ggvis(x = ~wt, y = ~mpg, size.brush := 400) %>%
#'   layer_points() %>%
#'   handle_brush(function(items, page_loc, session, ...) {
#'     show_tooltip(session, page_loc$r + 5, page_loc$t, html = nrow(items))
#'   })
handle_brush <- function(vis, on_move = NULL, fill = "black") {
  check_callback(on_move, c("items", "plot_loc", "page_loc", "session"))

  connect <- function(session, plot_id) {
    id <- paste0(plot_id, "_brush_move")
    shiny::observe({
      value <- session$input[[id]]
      if (is.null(value)) return()

      items <- tidy_items(value$items)
      page_loc <- list(
        t = value$pagey1, r = value$pagex2,
        b = value$pagey2, l = value$pagex1
      )
      plot_loc <- list(
        t = value$y1, r = value$x2,
        b = value$y2, l = value$x1
      )

      on_move(items = items, page_loc = page_loc, plot_loc = plot_loc,
        session = session)
    })
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
    v <- add_data(v, init, "ggvis_brush", add_suffix = FALSE)
    emit_rects(v, props(x := ~x, y := ~y,
      width := ~width, height := ~height,
      fill := fill, fillOpacity := 0.2,
      stroke := fill, strokeOpacity := 0.6,
      inherit = FALSE))
  })
}

tidy_items <- function(items) {
  if (length(items) == 0) {
    return(data.frame(keys__ = character()))
  }

  # FIXME: figure out more efficient way to do this
  dfs <- lapply(items, function(x) {
    class(x) <- "data.frame"
    attr(x, "row.names") <- .set_row_names(1L)
    x
  })
  items <- dplyr::rbind_all(dfs)

  if (is.numeric(items$key__)) {
    items$key__ <- as.character(items$key__ + 1)
  }
  items
}
