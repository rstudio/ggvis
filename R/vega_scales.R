# Given a gigvis object, return all needed vega scales, with correct
# domain values set.
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
      if (is.na(scale)) next
      
      type <- prop_type(data, prop, TRUE)
      scale_types[[scale]] <- c(scale_types[[scale]], type)
      
      use <- list(
        data = node$pipeline_id, 
        field = paste0("data.", prop_name(prop))
      )
      scale_uses[[scale]] <- c(scale_uses[[scale]], list(use))
    }
  }
  
  # Add in scales not already specified in spec
  needed <- setdiff(names(scale_types), names(scales))
  for (scale_n in needed) {
    type <- scale_types[[scale_n]][[1]]
    scales[[scale_n]] <- default_scale(scale_n, type)
  }

  # Override domains (if not already present)
  for (scale_n in names(scales)) {
    if (!is.null(scales[[scale_n]]$domain)) next
    scales[[scale_n]]$domain <- list(fields = scale_uses[[scale_n]])
  }
  
  unclass(unname(scales))
}
