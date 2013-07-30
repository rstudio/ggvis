# Given a gigvis mark object and set of scales, output a vega mark object
vega_mark <- function(mark) {
  # Keep only the vega-specific fields, then remove the class, drop nulls,
  # and convert to proper format for vega properties.
  defaults <- default_mark_properties(mark)
  props <- merge_props(defaults, mark$props)
  check_mark_props(mark, names(props))
  
  # Convert each property to a Vega-structured property
  vega_props <- Map(prop_vega, props, prop_to_scale(names(props)))
  
  list(
    type = mark$type,
    properties = list(
      update = vega_props
    ),
    from = list(data = mark$pipeline_id)
  )
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
