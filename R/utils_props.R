# Given a character vector like c("x", "x.update", "x.enter"), report which ones
# have .update, .enter, etc.
has_prop_event <- function(x) {
  sub("^.*\\.", "", x) %in% c("enter", "exit", "update", "hover", "brush")
}

# Remove the trailing event from a prop name
trim_prop_event <- function(x) {
  sub("\\.(enter|exit|update|hover|brush)$", "", x)
}

# Given a list with objects x.enter, x.update, y.update, return a list p of
# ggvis_props objects, with structure p$enter$x, p$update$x, p$update$y.
prop_event_sets <- function(props) {
  sets <- c("enter", "exit", "update", "hover", "brush")
  names(sets) <- sets

  x <- lapply(sets, function(set) {
    searchstr <- paste0("\\.", set, "$")
    matches <- props[grep(searchstr, names(props))]
    names(matches) <- trim_prop_event(names(matches))
    matches
  })

  compact(x)
}

# Given a props object, trim the .update, .enter, etc, and drop all those which
# are named in `drop`.
# drop_props(
#   props(x  = ~wt, x.enter = 0, stroke.enter := "black", stroke.hover := "red"),
#   c("stroke", "strokeOpacity")
# )
drop_props <- function(props, drop) {
  pnames <- trim_prop_event(names(props))
  props[!(pnames %in% drop)]
}

stroke_fill_defaults <- function(props, stroke = list(), fill = list()) {
  stroke_props <- merge_props(stroke, props)
  stroke_props <- drop_props(stroke_props, c("fill", "fillOpacity"))
  stroke_props <- merge_props(stroke_props, props(fill := "transparent"))

  fill_props <- merge_props(fill, props)
  fill_props <- drop_props(fill_props, c("stroke", "strokeOpacity"))
  fill_props <- merge_props(fill_props, props(stroke := "transparent"))

  list(stroke = stroke_props, fill = fill_props)
}

# @param props A props object.
# @param properties A character vector of properties to check.
# @param events A character vector of events that are not supported
# @param label A string naming the type of layer (like layer_bars), for error
#   messages.
check_unsupported_props <- function(props, properties, events, label = "") {
  unsupported <- vapply(props, function(p) {
    (p$property %in% properties) && (p$event %in% events)
  }, logical(1))

  if (any(unsupported)) {
    if (label == "") label <- "layer"
    stop(label, " presently cannot use properties ",
         paste(properties, collapse = ", "), " for ",
         paste(events, collapse = ", "), " events.")
  }
  invisible(NULL)
}
