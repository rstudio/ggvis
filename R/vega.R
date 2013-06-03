#' Given a gigvis object, output a vega object
#'
#'
#' @param envir The environment in which to evaluate the \code{data} parameter
#'   of the gigvis object.
#' @export
vega_spec <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  gv <- gigvis_fill_tree(gv, parent = NULL, envir = envir)
  mapped_vars <- gather_mapped_vars(gv)
  datasets <- gather_datasets(gv)
  datasets <- prune_datasets_columns(datasets, mapped_vars)

  scales <- gather_scales(gv, datasets)
  # scales <- vega_scales(gv$scales, gv$mapping, names(datasets)[2])

  # Convert data frames to vega format
  datasets <- lapply(names(datasets), function(name) {
    vega_df(datasets[[name]], name = name)
  })

  # These are key-values that only appear at the top level of the tree
  spec <- list(
    width = width,
    height = height,
    data = datasets,
    scales = scales,

    axes = list(list(type = "x", scale = "x"), list(type = "y", scale = "y")),
    padding = c(
      top = padding[1],
      right = padding[2],
      bottom = padding[3],
      left = padding[4]
    )
  )

  # Now deal with keys that also appear in lower levels of the tree, and merge
  # them in to the spec.
  spec <- c(spec, vega_process_node(node = gv, envir = envir))

  spec
}


# Recursively traverse tree and collect all the data sets used. Returns a flat
# list of data sets
gather_datasets <- function(node) {
  # Generate flat list of datasets, joining this node's data with children's
  datasets <-
    unlist(lapply(node$children, gather_datasets), recursive = FALSE)

  # Drop any duplicates (e.g., if two children have same data set)
  dups <- duplicated(names(datasets))
  if (any(dups)) {
    datasets <- datasets[!dups]
  }

  # Add this node's data set if not already present
  if (!is.null(node$data) && !(node$data %in% names(datasets))) {
    datasets[[node$data]] <- node$data_obj
  }

  datasets <- drop_nulls(datasets)

  datasets
}


# Given a gigvis node, recursively traverse tree and collect all mapped and
# split variables for all data sets used. Returns a flat named list of vectors,
# where the name of each vector is the name of the data set, and the content of
# each vector are strings naming each column used.
gather_mapped_vars <- function(node) {

  # Find all of children's mapped variables
  all_mapped_vars <-
    unlist(lapply(node$children, gather_mapped_vars), recursive = FALSE)

  # Add current node's splitting and mapped variables to the list
  vars <- unname(node$mapping)
  if (inherits(node$split, "split_by_group")) {
    vars <- unique(c(vars, node$split))
  }

  if (!is.null(node$data))
    all_mapped_vars[[node$data]] <- vars

  all_mapped_vars <- drop_nulls(all_mapped_vars)


  # Iterate over each data name, merging all entries that share the same name
  for (dataname in unique(names(all_mapped_vars))) {
    # Find all entries for this data frame
    matchidx <- names(all_mapped_vars) == dataname

    if (sum(matchidx) > 1) {
      # Append the vectors together, and drop duplicate entries
      mapped_vars <- unique(unlist(all_mapped_vars[matchidx]))

      # Drop all existing entries for this data set, then add back the new
      # single copy of it.
      all_mapped_vars[matchidx] <- NULL
      all_mapped_vars[[dataname]] <- mapped_vars
    }
  }

  all_mapped_vars
}

# Given a named list of data frames and a corresponding named list of mapped
# variables for each data frame,
prune_datasets_columns <- function(datasets, keep_vars) {
  if (length(datasets) != length(keep_vars) ||
      !identical(sort(names(datasets)), sort(names(keep_vars))) ) {
    stop("Names of datasets do not match names of sets of keep vars.")
  }

  for (name in names(datasets)) {
    datasets[[name]] <- prune_columns(datasets[[name]], keep_vars[[name]])
  }
  datasets
}


prune_columns <- function(data, keep_vars) UseMethod("prune_columns")

#' @S3method prune_columns split_data_dflist
prune_columns.split_data_dflist <- function(data, keep_vars) {
  structure(
    lapply(data, prune_columns, keep_vars),
    class = c("split_data_dflist", "split_data")
  )
}

#' @S3method prune_columns data.frame
prune_columns.data.frame <- function(data, keep_vars) {
  data[keep_vars]
}


# Recursively process nodes in the gigvis tree, and return corresponding vega
# tree.
#
# @param node A gigvis object node.
# @param envir Environment in which to evaluate \code{data}, to retrieve
#   the data object.
vega_process_node <- function(node, envir) {

  if (inherits(node, "mark")) {
    # Leaf nodes
    vega_node <- vega_mark(node)

  } else if (inherits(node, "gigvis_node")) {
    # Non-leaf nodes (including root node)
    vega_node <- list(
      marks = lapply(
        node$children,
        FUN = vega_process_node,
        envir = envir
      )
    )

    # For non-root, non-leaf nodes, add in grouping
    if (!inherits(node, "gigvis")) {

      vega_node$type <- "group"

      if (is.null(node$split)) {
        facet_keys <- NULL
      } else {
        facet_keys <- paste("data", node$split, sep = ".")
      }

      vega_node$from <- list(
        data = node$data,
        transform = list(list(
          type = "facet",
          keys = facet_keys
        ))
      )
    }
  }

  vega_node
}


vega_df <- function(x, name) {
  list(
    name = name,
    values = d3df(x)
  )
}



d3df <- function(x) UseMethod("d3df")

#' @S3method d3df data.frame
d3df.data.frame <- function(x) {
  n <- nrow(x)
  lapply(seq_len(n), function(i) as.list(x[i, ]))
}

#' @S3method d3df split_data_dflist
d3df.split_data_dflist <- function(x) {
  unlist(lapply(x, d3df), recursive = FALSE)
}
