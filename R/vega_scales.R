# Given a gigvis object, return vega scales.
find_scales <- function(x, nodes, data_table) {
  
  scales <- x$scales  
  
  # Loop through each node, recording the usage of each scale
  scale_types <- list()
  scale_uses <- list()
  for (node in nodes) {
    data <- isolate(data_table[[node$pipeline_id]]())
    for (prop_n in names(node$props)) {
      prop <- node$props[[prop_n]]
      scale <- prop_scale(prop, prop_n)
      
      type <- prop_type(data, prop)
      scale_types[[scale]] <- c(scale_types[[scale]], type)
      
      use <- list(data = node$pipeline_id, field = paste0("data.", prop_name(prop)))
      scale_uses[[scale]] <- c(scale_uses[[scale]], list(use))
    }
  }
  
  # Add in scales not already specified in spec
  needed <- setdiff(names(scale_types), names(scales))
  for (scale_n in needed) {
    type <- scale_types[[scale_n]][[1]]
    scales[[scale_n]] <- scale_defaults(scale_n, type)
  }
  
  # Associate each scale with the data values
  for (scale_n in names(scales)) {
    scales[[scale_n]]$domain <- list(fields = scale_uses[[scale_n]])
  }
  
  unclass(unname(scales))
}

scale_defaults <- function(scale, var_type) {
  if (var_type == "double") var_type <- "linear"
  
  if (scale == "x") {
    list(
      name   = scale,
      type   = var_type,
      range  = "width",
      zero   = FALSE,
      nice   = FALSE
    )
  } else if (scale == "y") {
    list(
      name   = scale,
      type   = var_type,
      range  = "height",
      zero   = FALSE,
      nice   = FALSE
    )

  } else if (scale == "stroke") {
    list(
      name   = scale,
      type   = var_type,
      range  = "category10"
    )

  } else if (scale == "fill") {
    list(
      name   = scale,
      type   = var_type,
      range  = "category10"
    )

  } else {
    stop("Unknown scale: ", scale)
  }
}
