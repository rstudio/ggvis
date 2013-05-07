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
    node$children <- lapply(node$children, FUN = gigvis_fill_tree, parent = node)
  }

  node
}


# Transform the data at each node into a standardized format.
# * Gets data from name in node$data
# * Converts data frame to have names x, y, color, etc.
# * Drops unused columns
# * Modifies `mapping` to reflect new, standardized column names
# * TODO: Calculates transforms
standardize_data <- function(node, envir) {
  if (!is.null(node$data)) {
    df <- get(node$data, envir = envir)

    # Keep only the columns are mapped to aesthetics
    df <- df[node$mapping]

    # Rename the columns to x, y, color, etc.
    names(df) <- names(node$mapping)

    # Store standardized data in node
    node$data_std <- df

    # Change mapping to c(x='x', y='y', color='color')
    node$mapping <- names(node$mapping)
    names(node$mapping) <- node$mapping
  }

  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = standardize_data, envir = envir)
  }

  node
}
