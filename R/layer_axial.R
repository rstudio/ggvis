#' Draw vertical or horizontal lines
#'
#' This adds axial lines to the plot, that is, lines that are parallel
#' to the axes. For each \code{x}, a vertical line spanning the
#' plot is created. Each \code{y} will result in a vertical line.
#' @inheritParams marks
#' @export
#' @examples
#' # In this example we will represent graphically the means of the
#' # mixture components of the faithful dataset.
#' # First we compute the means of each component:
#' library("dplyr")
#' means <- faithful %>%
#'   mutate(left = eruptions < 3) %>%
#'   group_by(left) %>%
#'   summarise(values = mean(eruptions))
#'
#' # Now we draw vertical lines corresponding to those means, plus an
#' # horizontal line 100 pixel from the top of the plot
#' ggvis(faithful) %>%
#'   layer_histograms(~eruptions) %>%
#'   layer_axial_lines(data = means,
#'     x = ~values, y := 100,
#'     stroke := "red", strokeWidth := 4
#'   )
layer_axial_lines <- function(vis, ..., data = NULL) {
  props <- props(..., env = parent.frame())
  props_names <- trim_prop_event(names(props))

  is_x <- "x" == props_names
  is_y <- "y" == props_names

  if ("x2" %in% props_names || "y2" %in% props_names) {
    stop("Cannot have x2 or y2 property", call. = FALSE)
  }
  if (sum(is_x) > 1 || sum(is_y) > 1) {
    stop("Cannot have more than one event for x and y", call. = FALSE)
  }

  if (any(is_x)) {
    x2_prop <- props[[which(is_x)]]
    x2_prop$property <- "x2"
    x2_prop_name <- paste0("x2.", x2_prop$event)
    v_props <- props[!is_y]
    v_props[[x2_prop_name]] <- x2_prop
    v_props$y <- prop("y", 0)
    v_props$y2 <- prop("y2", groupwise("height"))
    vis <- add_mark(vis, "rule", v_props, data, deparse2(substitute(data)))
  }

  if (any(is_y)) {
    y2_prop <- props[[which(is_y)]]
    y2_prop$property <- "y2"
    y2_prop_name <- paste0("y2.", y2_prop$event)
    h_props <- props[!is_x]
    h_props[[y2_prop_name]] <- y2_prop
    h_props$x <- prop("x", 0)
    h_props$x2 <- prop("x2", groupwise("width"))
    vis <- add_mark(vis, "rule", h_props, data, deparse2(substitute(data)))
  }

  vis
}
