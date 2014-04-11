#' Create a branch
#'
#' @export
branch <- function(vis, expr, env = parent.frame()) {
  # Save current data and props
  old_data  <- vis$cur_data
  old_props <- vis$cur_props

  fun <- defer(substitute(expr), env, quoted = TRUE)
  vis <- fun(vis)

  # Restore previous data and props
  vis$cur_data  <- old_data
  vis$cur_props <- old_props

  vis
}
