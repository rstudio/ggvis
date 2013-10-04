# Given a character vector like c("x", "x.update", "x.enter"), report which ones
# have .update, .enter, etc.
has_propset <- function(x) {
  sub("^.*\\.", "", x) %in% c("enter", "exit", "update", "hover")
}

# Remove the trailing propset from a prop name
trim_propset <- function(x) {
  sub("\\.(enter|exit|update|hover)$", "", x)
}

# Given a list with objects x.enter, x.update, y.update, return a list p of
# ggvis_props objects, with structure p$enter$x, p$update$x, p$update$y.
prop_sets <- function(props) {
  sets <- c("enter", "exit", "update", "hover")
  names(sets) <- sets

  x <- lapply(sets, function(set) {
    searchstr <- paste0("\\.", set, "$")
    matches <- props[grep(searchstr, names(props))]
    names(matches) <- trim_propset(names(matches))
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
  pnames <- trim_prop_attrib(names(props))
  props[!(pnames %in% drop)]
}
