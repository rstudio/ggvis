#' Create a ggvis visualization of a data object
#'
#'
#' @export
visualise <- function(data, props = NULL) {

  data_prefix <- deparse2(substitute(data))
  # Make sure data is reactive
  if (!is.reactive(data)) data <- as.reactive(data)

  data <- add_data_id(data, prefix = data_prefix)

  datalist <- list()
  datalist[[get_data_id(data)]] <- data

  proplist <- list()
  proplist[[props_id(props)]] <- props

  structure(
    list(
      data = datalist,
      props = proplist,
      cur_data = data,
      cur_props = props
    ),
    class = "ggvis"
  )
}


# Add a child node (layer or mark) to a ggvis object
add_child <- function(vis, child) {
  if (!is.ggvis(vis)) stop("Object to add child to is not a ggvis object.")

  vis$children <- c(vis$children, list(child))
  vis
}


add_legend <- function(vis, legend) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")

  vis$legends <- c(vis$legends, list(legend))
  vis
}

