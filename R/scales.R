#' Create a "scales" object.
#'
#' A scales object is used to manage multiple scales, essentially converting
#' a unnamed list into a named list.
#'
#' @export
#' @param ...,.scales scales to combine into a single scales object
#' @keywords internal
scales <- function(..., .scales = list()) {
  args <- c(list(...), .scales)
  if (length(args) == 0) return(NULL)
  stopifnot(all(vapply(args, is.scale, logical(1))))

  names(args) <- vapply(args, "name", FUN = `[[`, FUN.VALUE = character(1))

  structure(args, class = "scales")
}

#' @export
#' @rdname scales
#' @param x object to test for scales-ness
is.scales <- function(x) inherits(x, "scales")

#' @S3method format scales
format.scales <- function(x, ...) {
  paste("*", vapply(x, format, character(1)), collapse = "\n")
}

#' @S3method print scales
print.scales <- function(x, ...) cat(format(x, ...), "\n", sep = "")

# Merge two ggvis scales objects
#
# merge_scales(scales(scale("x", "linear")))
# merge_scales(scales(scale("x", "linear")), scales(scale("y", "linear")))
# merge_scales(scales(scale("x", "linear"), scale("y", "linear")),
#              scales(scale("y", "ordinal")))
merge_scales <- function(parent = NULL, child = NULL) {
  if (is.null(parent)) return(child)
  if (is.null(child)) return(parent)
  stopifnot(is.scales(parent), is.scales(child))

  structure(merge_vectors(parent, child), class = "scales")
}

# Given a ggvis object, return all needed vega scales, with correct
# domain values set.
add_default_scales <- function(x, nodes, data_table) {
  
  scales <- x$scales
  
  # Loop through each node, recording the usage of each scale
  scale_types <- list()
  scale_uses <- list()
  for (node in nodes) {
    data <- isolate(data_table[[node$pipeline_id]]())
    for (prop_n in names(node$props)) {
      prop <- node$props[[prop_n]]
      scale <- prop_scale(prop, prop_to_scale(trim_propset(prop_n)))
      if (is.na(scale)) next
      
      type <- prop_type(data, prop, processed = TRUE)
      scale_types[[scale]] <- c(scale_types[[scale]], type)
      
      use <- prop_domain(prop, node$pipeline_id)
      if (!is.null(use)) {
        scale_uses[[scale]] <- c(scale_uses[[scale]], list(use))
      }
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
    # If domain isn't specified (length == 0) or if only one of domainMin and
    # domainMax is specified, then get domain from data (min or max will also
    # be present). If both min and max are specified, no need to get domain from
    # data.
    if (length(scales[[scale_n]]$domain) < 2) {
      scales[[scale_n]]$domain <- list(fields = scale_uses[[scale_n]])
    }
  }
  
  unclass(scales)
}
