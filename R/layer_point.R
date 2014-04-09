#' @param vis A ggvis object, usually created by \code{\link{visualise}}.
#' @param props Props.
#'
#' @export
layer_point <- function(vis, props = NULL, data = NULL) {
  if (is.null(data)) {
    d_id <- vis$cur_data_id

  } else {
    if (!is.reactive(data))
      data <- reactive(data)

    # Register this data object in the vis object's data table
    d_id <- paste0(deparse2(substitute(data)), "_",
                   digest(data, algo = "crc32"))
    vis$data[[d_id]] <- data
  }


  # Register this set of props in the vis object's props table

  if (is.null(props)) {
    p_id <- vis$cur_props_id

  } else {
    parent_props <- vis$props[[vis$cur_data_id]]
    props <- merge_props(parent_props, props)

    p_id <- props_id(props)
    vis$props[[p_id]] <- props
  }

  add_child(vis, mark("symbol", props_id = p_id, data_id = d_id))
}
