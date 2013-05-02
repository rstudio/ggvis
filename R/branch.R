
# Need to figure out how to choose between transform and mark args
branch_smooth <- function(..., se = TRUE) {
  node(
    transform = transform_smooth(..., se = se),
    if (se) mark_ribbon(),
    mark_line()
  )
}
