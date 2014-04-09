flatten <- function(node, parent = NULL, session = NULL) {

  node$props <- init_inputs(node$props, session)
  # Convert handlers which were added directly to the ggvis object to reactives.
  # This is useful only for observers. If the reactive returns a reactive
  # expression, it will never get used because the returned values aren't
  # used.
  lapply(node$handlers, as.reactive, session)

  # Inherit behaviour from parent
  node$dynamic <- node$dynamic %||% parent$dynamic
  node$props <- merge_props(parent$props, node$props)

  # Create reactive pipeline, connected to parents
  if (empty(node$data)) {
    if (is.mark(node) && empty(parent$pipeline)) {
      stop("Node inherits data from parent, but parent has no data",
           call. = FALSE)
    }

    # Point to parent data
    node$pipeline <- parent$pipeline
    node$pipeline_id <- parent$pipeline_id
  } else {
    # Create new pipeline connected to parent
    node$pipeline <- connect(node$data, node$props, parent$pipeline, session)

    # Generate pipeline_id; if connected to parent's pipeline, append to its id
    id <- pipeline_id(node$data, node$props)
    if (!has_source(node$data)) {
      id <- paste(parent$pipeline_id, id, sep = "_")
    }
    node$pipeline_id <- id
  }

  if (is.mark(node)) {
    # Base case: so return self
    list(node)
  } else {
    # If there are any handlers that have layers, grab them and add to children.
    handler_layers <- lapply(node$handlers, extract_layer)
    node$children <- c(node$children, handler_layers)

    # Otherwise, recurse through children
    children <- lapply(node$children, flatten, parent = node, session = session)
    unlist(children, recursive = FALSE)
  }
}

# Given a list of layers, return a character vector of all data ID's used.
extract_data_ids <- function(layers) {
  data_ids <- vapply(layers,
    function(layer) get_data_id(layer$data),
    character(1)
  )
  unique(data_ids)
}

# Create a new reactive dataset containing only the data actually used
# by properties.
active_props <- function(data, layers) {
  # Collect all props for given data
  data_ids <- vapply(layers, function(layer) get_data_id(layer$data),
                     character(1))
  props <- lapply(layers, function(x) x$props)

  props_by_id <- split(props, data_ids)
  props_by_id <- lapply(props_by_id, unlist, recursive = FALSE)

  uprops_by_id <- lapply(props_by_id, function(props) {
    names <- vapply(props, prop_name, character(1))
    ok <- !duplicated(names) & names != ""

    setNames(props[ok], names[ok])
  })

  reactive_prop <- function(props, data) {
    force(props)
    force(data)
    reactive(apply_props(data(), props))
  }

  data_out <- new.env(parent = emptyenv())
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
apply_props.split_df <- function(data, props) {
  data[] <- lapply(data, apply_props, props)
  data
}
