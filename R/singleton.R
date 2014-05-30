#' Singleton.
#'
#' Use singleton when you want constant x or y position.
#'
#' @export
#' @examples
#' mtcars %>% ggvis("", ~mpg) %>%
#'   layer_points() %>%
#'   scale_nominal("x") %>%
#'   add_guide_axis("x", title = "", tick_size_major = 0)
#'
#' # OR
#' mtcars %>% ggvis("", ~mpg) %>%
#'   layer_points() %>%
#'   scale_singleton("x")
#'
#' # OR, even simpler
#' mtcars %>% ggvis(singleton(), ~mpg) %>% layer_points()
#'
#' # In the other direction:
#' mtcars %>% ggvis(~mpg, singleton()) %>% layer_points()
#' @export
singleton <- function() {
  structure("", class = "singleton")
}

#' @export
rep.singleton <- function(x, ...) {
  structure(NextMethod(), class = "singleton")
}
#' @export
print.singleton <- function(x, ...) cat("<singleton>\n")

#' @export
as.data.frame.singleton <- function(x, ...) {
  df <- list(x)
  attr(df, "row.names") <- .set_row_names(length(x))
  class(df) <- "data.frame"

  df
}

#' @export
vector_type.singleton <- function(x) "singleton"

#' @rdname singleton
#' @export
#' @inheritParams scale_nominal
scale_singleton <- function(vis, scale, points = TRUE) {
  vis <- scale_nominal(vis, domain = "", scale = scale, points = points)
  vis <- add_guide_axis(vis, scale, title = "", tick_size_major = 0)
  vis
}
