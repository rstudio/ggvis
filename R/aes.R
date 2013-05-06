#' @export
aes <- function(..., inherit = TRUE) {
  structure(c(...), inherit = inherit, class = "mapping")
}
