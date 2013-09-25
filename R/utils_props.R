# Given a character vector like c("x", "x.update", "x.enter"), report which ones
# have .update, .enter, etc.
has_prop_attrib <- function(x) {
  sub("^.*\\.", "", x) %in% c("enter", "exit", "update", "hover")
}

# Remove the trailing attribute from a prop name
trim_prop_attrib <- function(x) {
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
    names(matches) <- trim_prop_attrib(names(matches))
    matches
  })

  compact(x)
}
