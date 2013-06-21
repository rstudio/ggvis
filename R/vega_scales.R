# Given a gigvis object, get the scales

# Add scales: ensure that each node - either provided explicitly at that
# level or above, or assigned by the user. 
#
# First figure out which scales are needed (i.e. not found at )

# Build complete set of scales needed. For each property in each mark, need:
# * property name
# * scale name
# * dataset
# * variable type
# (but only if scale isn't already provided)

needed_scales <- function(node, provided = NULL) {
  provided <- c(provided, names(node$scales))
  
  # Base case, no children (so a mark)
  if (is.null(node$children)) {
    info <- prop_info(node)
    handled <- is.na(info$scale) | info$scale %in% provided
    return(info[handled, , drop = FALSE])
  }
  
  children <- lapply(node$children, needed_scales, provided = provided)
  do.call("rbind", children)
}

prop_info <- function(node, name = NULL) {
  if (is.null(name)) {
    all <- lapply(names(node$props), prop_info, node = node)
    return(do.call(rbind, all))
  }
  
  prop <- node$props[[name]]

  scale <- prop_scale(prop, default_scale(name))
  type <- prop_type(node$data_obj, prop, processed = TRUE)
  var <- prop_name(prop)
  
  data.frame(
    prop = name, 
    scale = scale, 
    var = var, 
    var_type = type, 
    data = node$data_id,
    stringsAsFactors = FALSE)
}

add_scales <- function(node, datasets) {
  needed <- needed_scales(node)
  
  by_scale <- split(needed, needed$scale)
  scales <- lapply(by_scale, function(x) {
    vega_scale(x$scale[[1]], x$var, x$data, x$var_type)
  })
  unname(scales)
}


# Given a gigvis scale, domain (the name of a source column, like 'mpg'), and
# name of data set, return a vega scale specification. Domain and data can be
# vectors with length > 1, though they must have the same length.
vega_scale <- function(scale, field, data, var_type) {
  var_type <- unique(var_type)
  if (length(var_type) > 1) {
    stop("Scale ", scale, " has multiple types of data: ", 
      paste0(var_type, collapse = ", "), call. = FALSE)
  }
  if (var_type == "null") return()
  
  if (length(field) != length(data)) {
    stop("field and data must be the same length.")
  }

  scale <- make_default_scale(scale, var_type)

  # These need to be added to all scales not just the ones we've added
  # automatically, so really need to go in a separate step after the
  # default scales have been added
  scale$domain <- list(fields = unname(Map(function(field, data) {
    list(data = data, field = paste0("data.", field))
  }, field, data)))
  
  scale
}

make_default_scale <- function(scale, var_type) {
  scale_type <- if (var_type %in% c("double", "integer")) "linear" else "ordinal"
  
  if (scale == "x") {
    list(
      name   = scale,
      type   = scale_type,
      range  = "width",
      zero   = FALSE,
      nice   = FALSE
    )
  } else if (scale == "y") {
    list(
      name   = scale,
      type   = scale_type,
      range  = "height",
      zero   = FALSE,
      nice   = FALSE
    )
    
  } else if (scale == "stroke") {
    list(
      name   = scale,
      type   = scale_type,
      range  = "category10"
    )
    
  } else if (scale == "fill") {
    list(
      name   = scale,
      type   = scale_type,
      range  = "category10"
    )
    
  } else {
    stop("Unknown scale: ", scale$name)
  }
}
