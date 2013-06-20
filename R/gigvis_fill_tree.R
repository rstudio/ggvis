# Given a gigvis object, fill out the tree.
#
# Returns a gigvis object in where each node has its own data set and
# aesthetic properties, and does not need to refer to its parent to find out any
# of this information.
#
# * First pass. After this pass, no need to refer to parents again
#   * Propagate name of the data set
#   * Retrieve the data set (as a data frame) and store in node$data_obj
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

    if (node$dynamic)
      symbol_table <- SymbolTable$new("data")

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

  if (node$dynamic) {
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
        data_obj <- split_data(data_obj, node$split)

        # Transform the data
        data_obj <- apply_transform(node$transform, data_obj, node$props)

        data_obj
      })
    }

  } else {
    pipe <- source_eager(parent$data_obj, name = parent$data_id)
    node$data <- c(as.pipeline(pipe), node$data)
    node$data_obj <- flow(node$data, node$props)

    # Give an id to the data object; this becomes the vega 'data' field
    node$data_id <- pipeline_id(node$data)
  }
  
  if (is.mark(node)) {
    # Base case: is a mark
    cols <- lapply(node$props, prop_value, data = node$data_obj)
    names(cols) <- vapply(node$props, prop_name, character(1))

    node$data_obj <- as.data.frame(compact(cols))
    node$data_id <- paste0(parent$data_id, "_", node$type)
  } else {
    # Fill in children recursively
    node$children <- lapply(node$children, FUN = gigvis_fill_tree,
      parent = node, envir = envir, symbol_table = symbol_table)
  }
    
  # If dynamic and this was the top node, add the symbol table as an attribute,
  # so that it can be returned to the caller.
  if (root_node && node$dynamic) {
    attr(node, "symbol_table") <- symbol_table$to_list()
  }

  node
}
