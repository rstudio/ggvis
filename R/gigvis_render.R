gigvis_render <- function(x, ...) {
  nodes <- flatten(x)
  data <- extract_data(nodes)
  
  # Create spec
  spec <- vega_spec(nodes, data, ...)
  
  list(spec = spec, data = data)
}

flatten <- function(node, parent = NULL) {
  
  # Inherit behaviour from parent
  node$dynamic <- node$dynamic %||% parent$dynamic
  node$props <- merge_props(parent$props, node$props)
  
  # Create reactive pipeline, connected to parents
  if (empty(node$data)) {
    if (empty(parent$pipeline)) {
      stop("Node inherits data from parent, but parent has no data", 
           call. = FALSE)
    }
    
    # Point to parent data
    node$pipeline <- parent$pipeline
    node$pipeline_id <- parent$pipeline_id
  } else {  
    # Create new pipeline connected to parent
    node$pipeline <- connect(node$data, node$props, parent$pipeline)
    node$pipeline_id <- paste0(
      parent$pipeline_id,
      pipeline_id(node$data, node$props)
    )
  }
  
  if (is.mark(node)) {
    # Base case: so return self
    list(node)
  } else {
    # Otherwise, recurse through children
    children <- lapply(node$children, flatten, parent = node)
    unlist(children, recursive = FALSE)
  }
}

extract_data <- function(nodes) {
  data_table <- new.env(parent = emptyenv())
  for (node in nodes) {
    id <- node$pipeline_id
    if (exists(id, data_table)) next
    
    data_table[[id]] <- node$data
  }
  
  data_table
}
