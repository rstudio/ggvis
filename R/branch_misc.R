#' A branch to add a line.
#'
#' @seealso \code{link{mark_line}}
#' @export
#' @param sort Should the data be sorted? If \code{TRUE} (the default), a
#'   \code{\link{transform_sort}} will be used. By default, this will sort the
#'   data on the x variable.
#' @param ... Named arguments are passed on to \code{\link{transform_sort}};
#'   unnamed arguments are not used.
#' @examples
#' ggvis(mtcars,
#'   props(x = ~wt, y = ~mpg, stroke = ~factor(cyl)),
#'   branch_line(),
#'   mark_symbol()
#' )
branch_line <- function(..., sort = TRUE) {
  comps <- parse_components(..., drop_named = TRUE)

  branch(
    auto_split(),
    if (sort) transform_sort(...),
    mark_line(comps$props)
  )
}
