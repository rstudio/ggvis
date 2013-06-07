# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic mappings, and does not need to refer to its parent to find out any
# of this information.
#
# * First pass. After this pass, no need to refer to parents again
#   * Propagate name of the data set
#   * Retrieve the data set (as a data frame) and store in node$data_obj
#   * Merge aesthetic mappings with the parent's aesthetics
#   * Split data (if needed)
#   * Transform data (if needed)
gigvis_fill_tree <- function(node, parent = NULL, envir = NULL) {
  if (is.null(parent))  parent <- list()

  # Handle data sets
  if (is.null(node$data)) {
    node$data <- parent$data
  }

  # If parent node isn't the root node, then inherit data (this is used when
  # generating the vega tree)
  if (inherits(parent, "gigvis")) {
    node$inherit_data <- FALSE
  } else {
    node$inherit_data <- TRUE
  }

  # Get data object:
  # - First check if parent has the data set (transformed data will be there)
  # - If not, then try to get data from envir
  if (!is.null(parent$data) && parent$data == node$data) {
    node$data_obj <- parent$data_obj
  } else if (is.null(node$data)) {
    node$data_obj <- NULL
  } else {
    node$data_obj <- get(node$data, envir = envir)
  }

  # Inherit mappings
  if (is.null(node$mapping)) {
    node$mapping <- parent$mapping

  } else {
    inherit_mapping <- attr(node$mapping, "inherit", exact = TRUE)

    if (is.null(inherit_mapping)) {
      stop("Aesthetic mappings must be created with aes().")

    } else if (inherit_mapping == TRUE) {
      node$mapping <- merge_vectors(parent$mapping, node$mapping)

    } else if (inherit_mapping == FALSE) {
      node$mapping <- parent$mapping
    }
  }

  # Split the data
  if (!is.null(node$split)) {
    node$data_obj <- split_data(node$data_obj, node$split)
  }

  # Transform the data
  if (!is.null(node$transform)) {
    node$data_obj <- apply_transform(node$data_obj, node$transform, node$mapping)

    # Rename the dataset with the transform type appended (e.g., "mtc" becomes
    # "mtc_smooth")
    node$data <- paste(node$data, transform_type(node$transform), sep = "_")
  }

  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = gigvis_fill_tree,
      parent = node, envir = envir)
  }

  node
}

# jcheng: This is a copy of gigvis_fill_tree that works for dynamic rendering
# (see comment above view_dynamic for what that means). The main difference
# is that data_obj isn't present, so I had to remove a bunch of code. I also
# don't understand split or transform so I just throw an error if those are
# present.
gigvis_fill_tree_dynamic <- function(node, parent = NULL) {
  if (is.null(parent))  parent <- list()
  
  # Handle data sets
  if (is.null(node$data)) {
    node$data <- parent$data
  }
  
  # If parent node isn't the root node, then inherit data (this is used when
  # generating the vega tree)
  if (inherits(parent, "gigvis")) {
    node$inherit_data <- FALSE
  } else {
    node$inherit_data <- TRUE
  }
  
  # Inherit mappings
  if (is.null(node$mapping)) {
    node$mapping <- parent$mapping
    
  } else {
    inherit_mapping <- attr(node$mapping, "inherit", exact = TRUE)
    
    if (is.null(inherit_mapping)) {
      stop("Aesthetic mappings must be created with aes().")
      
    } else if (inherit_mapping == TRUE) {
      node$mapping <- merge_vectors(parent$mapping, node$mapping)
      
    } else if (inherit_mapping == FALSE) {
      node$mapping <- parent$mapping
    }
  }
  
  # Split the data
  if (!is.null(node$split)) {
    stop("Dynamic split not implemented yet")

    node$data_obj <- split_data(node$data_obj, node$split)
  }
  
  # Transform the data
  if (!is.null(node$transform)) {
    stop("Dynamic transform not implemented yet")
    
    node$data_obj <- apply_transform(node$data_obj, node$transform, node$mapping)
    
    # Rename the dataset with the transform type appended (e.g., "mtc" becomes
    # "mtc_smooth")
    node$data <- paste(node$data, transform_type(node$transform), sep = "_")
  }
  
  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = gigvis_fill_tree_dynamic,
                            parent = node)
  }
  
  node
}
