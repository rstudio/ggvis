# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic mappings, and does not need to refer to its parent to find out any
# of this information.
#
# * First pass. After this pass, no need to refer to parents again
#   * Propagate name of the data set
#   * Merges aesthetic mappings with the parent's aesthetics
#
#
gigvis_fill_tree <- function(node, parent = NULL) {
  if (is.null(parent))  parent <- list()

  # Handle data sets
  if (is.null(node$data)) {
    if (is.null(node$transform)) {
      node$data <- parent$data
    } else {
      # TODO: figure out how to handle transformed data sets
    }

  } else {
    if (is.null(node$transform)) {
      if (!is.null(parent$data)) {
        stop("Node and parent can't both specify data set")
      }
      # Do nothing; use node$data as is.
    } else {
      stop("Node can't specify both data and transform")
    }
  }

  # Inherit mappings
  if (is.null(node$mapping)) {
    node$mapping <- parent$mapping

  } else {
    inherit_mapping <- attr(node$mapping, "inherit", exact = TRUE)

    if (inherit_mapping == TRUE) {
      node$mapping <- merge_vectors(parent$mapping, node$mapping)

    } else if (inherit_mapping == FALSE) {
      node$mapping <- parent$mapping

    }
  }

  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = gigvis_fill_tree, parent = node)
  }

  node
}
