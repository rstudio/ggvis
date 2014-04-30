branch <- function(vis, expr, env = parent.frame()) {
  fun <- defer(substitute(expr), env, quoted = TRUE)
  branch_f(vis, fun)
}


#' Create a branch, given a function
#'
#' @param vis The ggvis visualisation to modify.
#' @param fun A function that takes a single argument, the current
#'   visualisation as input, and returns a modified visualisation.
#' @export
branch_f <- function(vis, fun) {
  # Save current data and props
  old_data  <- vis$cur_data
  old_props <- vis$cur_props

  vis <- fun(vis)

  # Restore previous data and props
  vis$cur_data  <- old_data
  vis$cur_props <- old_props

  vis
}
