#' @param vis A ggvis object, usually created by \code{\link{visualise}}.
#' @param props Props.
#'
#' @export
layer_point <- function(vis, props = NULL, data = NULL) {
  if (is.null(data)) {
    data <- vis$cur_data

  } else {
    if (!is.reactive(data))
      data <- as.reactive(data)

    vis <- register_data(vis, data, prefix = deparse2(substitute(data)),
                         update_current = FALSE)
  }


  # Calculate the props for this layer
  if (is.null(props)) {
    props <- vis$cur_props

  } else {
    # Merge new props into parent, and then register the props with the vis
    props <- merge_props(vis$cur_props, props)
    vis$props[[props_id(props)]] <- props_id(props)
  }

  add_layer(vis, mark_point(props = props, data = data))
}
