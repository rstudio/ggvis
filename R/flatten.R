# Create a new reactive dataset containing only the data actually used
# by properties.
active_props <- function(data, layers) {
  # Collect all props for given data
  data_ids <- vapply(layers, function(layer) data_id(layer$data),
    character(1))
  props <- lapply(layers, function(x) x$props)

  props_by_id <- split(props, data_ids)
  props_by_id <- lapply(props_by_id, unlist, recursive = FALSE)

  uprops_by_id <- lapply(props_by_id, function(props) {
    names <- vapply(props, prop_name, character(1))
    ok <- !duplicated(names) & names != ""

    setNames(props[ok], names[ok])
  })

  reactive_prop <- function(props, parent_data) {
    force(props)
    force(parent_data)
    reactive(apply_props(parent_data(), props))
  }

  data_out <- list()
  for (data_n in names(uprops_by_id)) {
    data_out[[data_n]] <- reactive_prop(uprops_by_id[[data_n]], data[[data_n]])
  }

  data_out
}

# Apply properties to a data object, creating calculated columns and dropping
# unused columns.
apply_props <- function(data, props) {
  UseMethod("apply_props")
}

#' @export
apply_props.data.frame <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  names(cols) <- vapply(props, prop_name, character(1))

  quickdf(cols)
}

#' @export
apply_props.grouped_df <- function(data, props) {
  dplyr::do(data, apply_props(., props))
}
