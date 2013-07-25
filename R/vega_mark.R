# Given a gigvis mark object and set of scales, output a vega mark object
vega_mark <- function(node, scales) {

  # Generate the fields related to mappings (x, y, etc)
  vega_node <- list(
    type = node$type,
    properties = list(
      update = vega_mark_properties(node, scales)
    )
  )

  if (!node$inherit_data) {
    vega_node$from <- list(data = node$data_id)
  }

  vega_node
}

# Given a gigvis mark object and set of scales, return a list of vega mark properties
vega_mark_properties <- function(mark, scales) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)
  check_mark_props(mark, names(props))

  # Convert each property to a Vega-structured property
  mapply(prop = names(props), val = props, MoreArgs = list(scales = scales),
    FUN = vega_mark_property, SIMPLIFY = FALSE)
}

#' @importFrom utils adist
check_mark_props <- function(mark, props) {
  valid <- valid_mark_properties(mark)

  invalid <- setdiff(props, valid)
  if (length(invalid) == 0) return(invisible(TRUE))

  ldist <- adist(invalid, valid, ignore.case = TRUE, partial = FALSE,
                 costs = c(ins = 0.5, sub = 1, del = 2))
  
  closest <- apply(ldist, 1, min)
  possible_match <- closest < 5
  if (any(possible_match)) {
    best <- apply(ldist, 1, which.min)
    
    matches <- valid[best][possible_match]  
    suggest <- paste0("Did you mean: ", paste0(matches, collapse = ", "), "?")
  } else {
    suggest <- ""
  }

  stop("Unknown properties: ", paste0(invalid, collapse = ", "), ".\n", suggest,
       call. = FALSE)
}

vega_mark_property <- function(prop, val, scales) {
  # Convert scales to a named list for convenience
  names(scales) <- vapply(scales, `[[`, "name", FUN.VALUE = character(1))

  vega <- prop_vega(val, prop_to_scale(prop))

  # This is an ugly hack, but not sure yet how to make better.
  if ((prop == "width"  && scales$x$type == "ordinal") ||
      (prop == "height" && scales$y$type == "ordinal")) {
    vega$band <- TRUE
  }

  vega
}
