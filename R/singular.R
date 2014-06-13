#' singular.
#'
#' Use singular when you want constant x or y position.
#'
#' @export
#' @examples
#' mtcars %>% ggvis("", ~mpg) %>%
#'   layer_points() %>%
#'   scale_nominal("x") %>%
#'   add_axis("x", title = "", tick_size_major = 0)
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
scale_singular <- function(vis, property, name = property, label = name,
                           points = TRUE, domain = NULL, override = NULL) {
  # Some of the arguments are ignored; they're there to provide a consistent
  # interface with other scales
  vis <- scale_nominal(vis, domain = "", property = property, name = name,
                           label = "", points = points, override = override)
  vis <- add_axis(vis, property, tick_size_major = 0)
  vis
}
