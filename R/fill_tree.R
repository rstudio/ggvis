# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic mappings, and does not need to refer to its parent to find out any
# of this information.
#
# * First pass. After this pass, no need to refer to parents again
#   * Propagate name of the data set
#   * Retrieve the data set (as a data frame) and store in node$data_df
#   * Merges aesthetic mappings with the parent's aesthetics
#
#
gigvis_fill_tree <- function(node, parent = NULL, envir = NULL) {
  if (is.null(parent))  parent <- list()

  # Handle data sets
  if (is.null(node$data)) {
    node$data <- parent$data
  }
  # Store data frame in node
  node$data_df <- get(node$data, envir = envir)

  if (!is.null(node$transform)) {
    node$data <- compute(node$transform, node$data)
  }

  # Inherit mappings
  if (is.null(node$mapping)) {
    node$mapping <- parent$mapping

  } else {
    inherit_mapping <- attr(node$mapping, "inherit", exact = TRUE)

    if (is.null(inherit_mapping)) {
      stop("Aesthetic mappings must be create with aes().")

    } else if (inherit_mapping == TRUE) {
      node$mapping <- merge_vectors(parent$mapping, node$mapping)

    } else if (inherit_mapping == FALSE) {
      node$mapping <- parent$mapping

    }
  }


  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = gigvis_fill_tree,
      parent = node, envir = envir)
  }

  node
}
