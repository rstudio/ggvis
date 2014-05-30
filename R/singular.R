#' singular.
#'
#' Use singular when you want constant x or y position.
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
#'   scale_singular("x")
#'
#' # OR, even simpler
#' mtcars %>% ggvis(singular(), ~mpg) %>% layer_points()
#'
#' # In the other direction:
#' mtcars %>% ggvis(~mpg, singular()) %>% layer_points()
#' @export
singular <- function() {
  structure("", class = "singular")
}

#' @export
rep.singular <- function(x, ...) {
  structure(NextMethod(), class = "singular")
}
#' @export
print.singular <- function(x, ...) cat("<singular>\n")

#' @export
as.data.frame.singular <- function(x, ...) {
  df <- list(x)
  attr(df, "row.names") <- .set_row_names(length(x))
  class(df) <- "data.frame"

  df
}

#' @export
vector_type.singular <- function(x) "singular"

#' @rdname singular
#' @export
#' @inheritParams scale_nominal
scale_singular <- function(vis, scale, points = TRUE) {
  vis <- scale_nominal(vis, domain = "", scale = scale, points = points)
  vis <- add_guide_axis(vis, scale, title = "", tick_size_major = 0)
  vis
}
