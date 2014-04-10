#' Visualise a data set with a ggvis graphic.
#'
#' @import assertthat
#' @export
ggvis <- function(data, props = NULL) {

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
      marks = list(),
      data = datalist,
      props = proplist,
      cur_data = data,
      cur_props = props
    ),
    class = "ggvis"
  )
}

#' Is an object a ggvis object?
#'
#' @export
#' @param x an object to test
#' @keywords internal
is.ggvis <- function(x) inherits(x, "ggvis")


# Add a mark to a ggvis object.
add_mark <- function(vis, type = NULL, props = NULL, data = NULL,
                     data_name = "unnamed_data") {

  # Get the data object and register it if necessary
  if (is.null(data)) {
    data <- vis$cur_data

  } else {
    if (!is.reactive(data)) {
      data <- as.reactive(data)
    }
    vis <- register_data(vis, data, prefix = data_name, update_current = FALSE)
  }


  # Calculate the props for this layer
  new_props <- merge_props(vis$cur_props, props)

  # Register the props with the vis if needed
  if (!is.null(props)) {
    vis <- register_props(vis, new_props)
  }

  vis$marks <- c(vis$marks, list(mark(type, props = new_props, data = data)))
  vis
}


add_legend <- function(vis, legend) {
  if (!is.ggvis(vis)) stop("Object to add legend to is not a ggvis object.")

  vis$legends <- c(vis$legends, list(legend))
  vis
}


# Register a data object in the ggvis object's data list.
# This adds a data_id attribute to the data object, with the specified prefix.
#
# @param vis A ggvis object.
# @param data A reactive data object.
# @param prefix A prefix for the data ID.
# @param update_current Should the cur_data field be updated to this data object?
register_data <- function(vis, data, prefix = "unnamed_data",
                     update_current = FALSE) {

  data <- add_data_id(data, prefix)
  vis$data[[get_data_id(data)]] <- data

  if (update_current) {
    vis$cur_data <- data
  }

  vis
}


# Register a property set object in the ggvis object's props list.
# @param vis A ggvis object.
# @param props A props object.
# @param update_current Should the cur_props field be updated to this props object?
register_props <- function(vis, props, update_current = FALSE) {
  vis$props[[props_id(props)]] <- props

  if (update_current) {
    vis$cur_props <- props
  }

  vis
}


#' Tools to save and view static specs.
#'
#' These functions are mainly useful for testing.
#'
#' @param path location to save spec to, or load spec from
#' @param x a ggvis object
#' @param ... other arguments passed to \code{as.vega}
#' @keywords internal
#' @export
save_spec <- function(path, x = last_vis(), ...) {
  assert_that(is.ggvis(x), is.string(path))

  json <- toJSON(as.vega(x, ...), pretty = TRUE)
  writeLines(json, path)
}

#' @importFrom RJSONIO fromJSON
#' @rdname save_spec
view_spec <- function(path, ...) {
  contents <- paste0(readLines(path), collapse = "\n")
  spec <- fromJSON(contents)
  view_static(spec)
}
