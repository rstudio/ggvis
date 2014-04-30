#' Transformation: stack the data
#'
#' \code{transform_stack} is a data transformation that stacks values in a
#' a data object. Typically, y values are stacked at each unique x value,
#' although it's also possible to stack x values at each unique y.
#'
#' @section Input:
#' \code{transform_stack} takes a data frame or a split_df as input.
#'
#' @section Ouput:
#'
#' \code{transform_stack} returns a sorted data frame or split_df with the same
#'   columns as the input as well as columns named \code{ymin__} and
#'   \code{ymax__} (or \code{xmin__} and \code{xmax__} if stacking x
#'   values). These columns specify the upper and lower bounds of each stacked
#'   object.
#'
#'  Note that \code{transform_stack} does not sort the values. If you want to
#'  sort on another variable, you can use \code{\link{transform_sort}} before
#'  stacking. This is useful when, for example, your data is unsorted and you
#'  want stacked bar chart where each stack of bars appears in the same order.
#'  Also, if you use \code{by_group}, it will result in the data being sorted
#'  by the grouping variables.
#'
#' @param direction The direction to stack. This is a variable name after
#'   mapping, and must be either \code{"y"} (the default) or \code{"x"}.
#'   For example, with \code{props(y = ~mpg)}, you would use \code{"y"}, not
#'   \code{"mpg"}.
#' @examples
#' # Collapse across hair & eye colour data across sex
#' hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))
#'
#' # Without stacking - bars overlap
#' ggvis(hec,
#'   props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5),
#'   dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
#'   mark_rect(props(y2 = 0, width = band()))
#' )
#'
#' # With stacking
#' ggvis(hec, transform_stack(),
#'   props(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5),
#'   dscale("x", "nominal", range = "width", padding = 0, points = FALSE),
#'   mark_rect(props(y = ~ymin__, y2 = ~ymax__, width = band()))
#' )
#'
#' # Stacking in x direction instead of default y
#' ggvis(hec, transform_stack(direction = "x"),
#'   props(x = ~Freq, y = ~Hair, fill = ~Eye, fillOpacity := 0.5),
#'   dscale("y", "nominal", range = "height", padding = 0, points = FALSE),
#'   mark_rect(props(x = ~xmin__, x2 = ~xmax__, height = band()))
#' )
#'
#'
#' # Stack y values at each x
#' sluice(pipeline(hec, transform_stack()), props(x = ~Hair, y = ~Freq))
#'
#' # Same effect, but this time stack x values at each y
#' sluice(pipeline(hec, transform_stack(direction = "x")),
#'   props(x = ~Freq, y = ~Hair))
#'
#' @export
transform_stack <- function(direction = "y") {
  if (length(direction) != 1) stop("direction must have 1 item, 'x' or 'y'")
  if (direction != "y" && direction != "x") {
    stop("direction for transform_stack must be 'x' or 'y'.")
  }

  transform("stack", direction = direction)
}


#' Stack overlapping data.
#'
#' @export
#' @param x A data object
#' @param stack_var A string specifying the stacking variable.
#' @param group_var A string specifying the grouping variable.
#' @examples
#' mtcars %>% cbind(count = 1) %>% compute_stack(count, cyl)
#'
#' # Shouldn't use or affect existing grouping
#' mtcars %>% cbind(count = 1) %>% group_by(am) %>% compute_stack(count, cyl)
#'
#' If given a ggvis object, will use x variable for stacking by default
#' mtcars %>% ggvis(x = ~cyl, y = ~wt) %>%
#'   compute_stack() %>%
#'   mark_rect(props(x = ~cyl - 0.5, x2 = ~cyl + 0.5,
#'                   y = ~stack_upr_, y2 = ~stack_lwr_))
compute_stack <- function(x, stack_var = NULL, group_var = NULL, ...) {
  UseMethod("compute_stack")
}

#' @export
compute_stack.grouped_df <- function(x, stack_var = NULL, group_var = NULL, ...) {
  # Save original groups, and restore after stacking
  old_groups <- dplyr::groups(x)
  x <- dplyr::ungroup(x)

  compute_stack(x, stack_var, group_var)

  dplyr::regroup(x, old_groups)
}

#' @export
compute_stack.data.frame <- function(x, stack_var = NULL, group_var = NULL, ...) {
  x <- do_call(dplyr::group_by, quote(x), group_var)

  # FIXME: This is a workaround for dplyr issue #412
  lag <- dplyr::lag

  # FIXME: mutate evaluates in this function's environment, which isn't right.
  # This is like mutate(x, stack_upr_ = cumsum(stack_var),
  #                     stack_lwr_ = lag(stack_upr_))
  # but with value of stack_var in the right place.
  x <- do_call(dplyr::mutate, quote(x),
    stack_upr_ = call("cumsum", stack_var),
    stack_lwr_ = quote(lag(stack_upr_, default = 0))
  )

  x <- dplyr::ungroup(x)
}

#' @export
compute_stack.ggvis <- function(x, stack_var = NULL, group_var = NULL, ...) {
  # Try to figure out the stack_var and group_var, if not explicitly
  if (is.null(stack_var)) {
    if (!is.null(x$cur_props) &&
        !is.null(x$cur_props$y.update) &&
        x$cur_props$y.update$type == "variable") {

      stack_var <- x$cur_props$y.update$value

    } else {
      stop("Need stack_var or a variable y.update property")
    }
  }

  if (is.null(group_var)) {
    if(!is.null(x$cur_props) &&
       !is.null(x$cur_props$x.update) &&
       x$cur_props$x.update$type == "variable") {

      group_var <- x$cur_props$x.update$value

    } else {
      stop("Need group_var or a variable x.update property")
    }
  }

  parent_data <- x$cur_data

  new_data <- reactive({
    data <- parent_data()
    do_call("compute_stack", quote(data), quote(stack_var), quote(group_var))
  })

  register_data(x, new_data, "compute_stack")
}
