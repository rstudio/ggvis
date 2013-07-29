# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic properties, and does not need to refer to its parent to find out any
# of this information.
#
# @param node The gigvis node to operate on.
# @param parent The parent node.
# @return a list of nodes
#' @importFrom digest digest
#' 
#' p <- gigvis("mtcars", props(x ~ wt, y ~ mpg), mark_symbol())
#' gigvis_flatten(p)
#' p <- gigvis("mtcars", props(x ~ wt, y ~ mpg), node(node(mark_symbol())))
#' gigvis_flatten(p)
gigvis_flatten <- function(node, parent = NULL) {

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
    children <- lapply(node$children, gigvis_flatten, parent = node)
    unlist(children, recursive = FALSE)
  }
}
