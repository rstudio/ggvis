#' @export
branch_brush <- function(...) {
  comps <- parse_components(..., drop_named = TRUE)

  props <- merge_props(
    props(x := ~x, y := ~y, width := ~width, height := ~height,
          fill := "black", fillOpacity := 0.3,
          stroke := "black", strokeOpacity := 0.6),
    comps$props
  )

  mark_rect(
    props,
    data = pipeline(
      data.frame(x = 0, y = 0, width = 0, height = 0),
      .id = "ggvis_brush"
    )
  )
}

# Return TRUE if any of the nodes use a .brush prop
has_brush_props <- function(nodes) {
  props <- unlist(lapply(nodes, function(x) names(x$props)), recursive = FALSE)
  any(grepl(".brush$", props))
}
