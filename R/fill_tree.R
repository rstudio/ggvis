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
#
# @param node The gigvis node to operate on.
# @param parent The parent node.
# @param envir Environment in which to look for data object.
# @param dynamic Should this be prepared for dynamic data? If so, the data
#   object will _not_ be embedded; instead a symbol referring to the data will
#   be embedded, and the data itself will be sent later.
# @param symbol_table A table of data symbols, used only when \code{dynamic=TRUE}.
#' @importFrom digest digest
gigvis_fill_tree <- function(node, parent = NULL, envir = NULL,
                             dynamic = FALSE, symbol_table = NULL) {

  # If we're at the top of the tree, initialize some data structures
  if (is.null(parent)) {
    parent <- list()

    if (dynamic)
      symbol_table <- SymbolTable$new("data")
  }

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


  if (dynamic) {
    # For dynamic, add the data to the symbol table
    if (!is.null(node$data)) {
      data_ref <- node$data

      # The gv object is full of data=function() {...}; crawl over the tree and
      # replace each of those with a synthetic ID, and return the transformed tree.
      # The transformed tree will also have a list that maps the synthetic IDs to
      # their functions; it will be made available as the attribute "symbol_table".
      # Later on, in view_dynamic, this table will be used to create observers that
      # send data to the client, where they will be plugged into the appropriate
      # chart.
      # Check whether this data ref is already in the symbol table; if not, add
      # it to the table.
      if (!symbol_table$contains(data_ref)) {
        data_ref <- symbol_table$add_item(data_ref)
      }

      # Save a function which will return the data, properly split and
      # transformed
      node$data <- symbol_table$add_item(function() {
        # First search in the symbol table for the data_ref, before searching
        # the usual places with find_data_object
        if (symbol_table$contains(data_ref))
          data_ref <- symbol_table$get(data_ref)

        data_obj <- find_data_object(data_ref, envir = envir)

        # Split the data
        if (!is.null(node$split)) {
          data_obj <- split_data(data_obj, node$split)
        }

        # Transform the data
        if (!is.null(node$transform)) {
          data_obj <- apply_transform(data_obj, node$transform, node$mapping)
        }

        data_obj
      })
    }

  } else {
    # For non-dynamic, get data object:
    # - First check if parent has the data set (transformed data will be there)
    # - If not, then try to get data from envir
    if (!is.null(parent$data) && parent$data == node$data) {
      node$data_obj <- parent$data_obj
    } else if (is.null(node$data)) {
      node$data_obj <- NULL
    } else {
      node$data_obj <- get(node$data, envir = envir)
    }

    # Split the data
    if (!is.null(node$split)) {
      node$data_obj <- split_data(node$data_obj, node$split)
    }

    # Transform the data
    if (!is.null(node$transform)) {
      node$data_obj <- apply_transform(node$data_obj, node$transform, node$mapping)

      # Rename the dataset with the transform type and hashed transform appended
      # (e.g., "mtc" becomes "mtc_smooth_asdf842af3")
      # The hashing is done so that if there are multiple transforms of the
      # same type, they won't interfere. For example, if there are two smooth
      # transforms with different methods ("lm" and "loess") or with different
      # mappings, they should result in different data names.
      node$data <- paste(
        node$data,
        transform_type(node$transform),
        digest::digest(node[c("transform", "mapping")], algo = "crc32"),
        sep = "_")
    }
  }


  # Fill in children recursively
  if (!is.null(node$children)) {
    node$children <- lapply(node$children, FUN = gigvis_fill_tree,
      parent = node, envir = envir, dynamic = dynamic,
      symbol_table = symbol_table)
  }

  # If dynamic and this was the top node, add the symbol table as an attribute,
  # so that it can be returned to the caller.
  if (dynamic && identical(parent, list())) {
    attr(node, "symbol_table") <- symbol_table$to_list()
  }

  node
}
