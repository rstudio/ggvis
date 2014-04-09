#' Create a ggvis visualization of a data object
#'
#'
#' @export
visualise <- function(data, props = NULL) {
  
  d_id <- paste0(deparse2(substitute(data)), "_",
                 digest(data, algo = "crc32"))

  # Make sure data is reactive
  if (!is.reactive(data)) data <- reactive(data)

  datalist <- list()
  datalist[[d_id]] <- data


  p_id <- props_id(props)

  proplist <- list()
  proplist[[p_id]] <- props

  structure(
    list(
      data = datalist,
      cur_data_id = d_id,
      props = proplist,
      cur_props_id = p_id
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
