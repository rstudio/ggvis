#' Create a branch, given an expression
#'
#' @export
branch <- function(vis, expr, env = parent.frame()) {
  fun <- defer(substitute(expr), env, quoted = TRUE)
  vis %>% branch_f(fun)
}


#' Create a branch, given a function
#'
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
