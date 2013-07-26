# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic properties, and does not need to refer to its parent to find out any
# of this information.
#
# * First pass. After this pass, no need to refer to parents again
#   * Propagate name of the data set
#   * Merge aesthetic properties with the parent's aesthetics
#   * Split data (if needed)
#   * Transform data (if needed)
#
# @param node The gigvis node to operate on.
# @param parent The parent node.
# @param envir Environment in which to look for data object.
# @param symbol_table A table of data symbols, used only when \code{dynamic=TRUE}.
#' @importFrom digest digest
gigvis_fill_tree <- function(node, parent = NULL, envir = NULL,
                             symbol_table = NULL) {
  # If we're at the top of the tree, initialize some data structures
  if (is.null(parent)) {
    root_node <- TRUE
    parent <- list()

  } else {
    root_node <- FALSE
  }

  if (is.null(node$dynamic))  node$dynamic <- parent$dynamic
  if (is.null(node$data))  node$data <- parent$data

  # If parent node is NOT the root node, then inherit data (this is used when
  # generating the vega tree)
  if (inherits(parent, "gigvis")) {
    node$inherit_data <- FALSE
  } else {
    node$inherit_data <- TRUE
  }

  # Inherit properties from parent
  node$props <- merge_props(parent$props, node$props)

  # Connect this node's data pipeline to the parent's
  node$pipeline <- c(as.pipeline(parent$data), node$data)

  # Create the reactive expression which will yield the data
  node$data <- connect(node$pipeline, node$props)

  # Give an id to the data object; this becomes the vega 'data' field.
  node$data_id <- pipeline_id(node$pipeline, node$props)

  if (!is.null(node$children)) {
    # Fill in children recursively
    node$children <- lapply(node$children, FUN = gigvis_fill_tree,
      parent = node, envir = envir, symbol_table = symbol_table)
  }

  node
}
