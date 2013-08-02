# p <- gigvis(mtcars, props(x ~ disp, y ~ mpg), mark_symbol(), branch_smooth(n = input_slider(10, 100)))
# controls(p)


controls <- function(x, ...) UseMethod("controls")

# Since base gigvis object inherits from `node`, should be able to call
# controls on a gigvis plot and get a list of all controls
# 
# Assumes that controls have a meaningful id function which allows us
# to remove duplicates which will occur if you've bound the same delayed
# reactive to multiple places.

controls.gigvis_node <- function(x, ...) {
  t_controls <- unlist(lapply(x$data, controls), recursive = FALSE)
  p_controls <- unlist(lapply(x$props, controls), recursive = FALSE)
  c_controls <- unlist(lapply(x$children, controls), recursive = FALSE)
  
  all <- compact(c(t_controls, p_controls, c_controls))
  ids <- vapply(all, function(x) attr(x, "id"), character(1))
  all[!duplicated(ids)]
}

controls.list <- function(x, ...) {
  dr <- vapply(x, is.delayed_reactive, logical(1))
  unlist(lapply(x[dr], controls), recursive = FALSE, use.names = FALSE)
}
controls.gigvis_props <- controls.list
controls.transform <- controls.list

controls.delayed_reactive <- function(x, ...) {
  # Assuming each reactive only provides one control
  list(x$controls)
}
controls.default <- function(x, ...) NULL