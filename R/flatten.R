flatten <- function(node, parent = NULL, session = NULL) {

  node$props <- init_inputs(node$props, session)

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
    # Otherwise, recurse through children
    children <- lapply(node$children, flatten, parent = node, session = session)
    unlist(children, recursive = FALSE)
  }
}

extract_data <- function(nodes) {
  data_table <- new.env(parent = emptyenv())
  for (node in nodes) {
    id <- node$pipeline_id
    if (exists(id, data_table)) next

    data_table[[id]] <- node$pipeline
  }

  data_table
}

# Create a new reactive dataset containing only the data actually used
# by properties.
active_props <- function(data, nodes) {
  # Collect all props for given data
  pipeline_id <- vapply(nodes, function(x) x$pipeline_id, character(1))
  props <- lapply(nodes, function(x) x$props)

  props_by_id <- split(props, pipeline_id)
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

#' @S3method apply_props data.frame
apply_props.data.frame <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  names(cols) <- vapply(props, prop_name, character(1))

  quickdf(compact(cols))
}

#' @S3method apply_props split_df
apply_props.split_df <- function(data, props) {
  data[] <- lapply(data, apply_props, props)
  data
}
