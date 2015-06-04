#' Stack overlapping data.
#'
#' @return A data frame with columns:
#'  \item{stack_upr_}{the lower y coordinate for a stack bar}
#'  \item{stack_lwr_}{the upper y coordinate for a stack bar}
#' @export
#' @param x A data object
#' @param stack_var A string specifying the stacking variable.
#' @param group_var A string specifying the grouping variable.
#' @examples
#' mtcars %>% cbind(count = 1) %>% compute_stack(~count, ~cyl)
#'
#' # Shouldn't use or affect existing grouping
#' mtcars %>% cbind(count = 1) %>% group_by(am) %>% compute_stack(~count, ~cyl)
#'
#' # If given a ggvis object, will use x variable for stacking by default
#' mtcars %>% ggvis(x = ~cyl, y = ~wt) %>%
#'   compute_stack(stack_var = ~wt, group_var = ~cyl) %>%
#'   layer_rects(x = ~cyl - 0.5, x2 = ~cyl + 0.5, y = ~stack_upr_,
#'     y2 = ~stack_lwr_)
#'
#' # Collapse across hair & eye colour data across sex
#' hec <- as.data.frame(xtabs(Freq ~ Hair + Eye, HairEyeColor))
#' hec %>% compute_stack(~Freq, ~Hair)
#'
#' # Without stacking - bars overlap
#' hec %>% ggvis(~Hair, ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
#'   layer_rects(y2 = 0, width = band())
#'
#' # With stacking
#' hec %>% ggvis(x = ~Hair, y = ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
#'   compute_stack(~Freq, ~Hair) %>%
#'   layer_rects(y = ~stack_lwr_, y2 = ~stack_upr_, width = band())
#'
#' # layer_bars stacks automatically:
#' hec %>% ggvis(~Hair, ~Freq, fill = ~Eye, fillOpacity := 0.5) %>%
#'   group_by(Eye) %>%
#'   layer_bars(width = 1)
compute_stack <- function(x, stack_var = NULL, group_var = NULL) {
  UseMethod("compute_stack")
}

#' @export
compute_stack.grouped_df <- function(x, stack_var = NULL, group_var = NULL) {
  assert_that(is.formula(stack_var), is.formula(group_var))

  # Save original groups, and restore after stacking
  old_groups <- dplyr::groups(x)

  x <- dplyr::ungroup(x)
  x <- compute_stack(x, stack_var, group_var)
  x <- dplyr::group_by_(x, .dots = old_groups)

  x
}

#' @export
compute_stack.data.frame <- function(x, stack_var = NULL, group_var = NULL) {
  assert_that(is.formula(stack_var), is.formula(group_var))

  # Round grouping variable to 8 significant digits
  gvar <- lazyeval::interp(~round_fp(x), x = group_var[[2]])
  x <- dplyr::group_by_(x, group__ = gvar)

  # FIXME: mutate evaluates in this function's environment, which isn't right.
  # This is like mutate(x, stack_upr_ = cumsum(stack_var[[2]]),
  #                     stack_lwr_ = lag(stack_upr_, default = 0))
  # but with value of stack_var in the right place.

  # dplyr 0.4.2 overrides lag instead of overriding default method AND
  # preserves attributes so we don't want to use base lag
  if (packageVersion("dplyr") < "0.4.2") {
    args <- list(
      stack_upr_ = bquote(cumsum(.(stack_var[[2]]))),
      stack_lwr_ = bquote(lag(stack_upr_, default = 0))
    )
  } else {
    args <- list(
      stack_upr_ = bquote(cumsum(.(stack_var[[2]]))),
      stack_lwr_ = bquote(dplyr::lag(stack_upr_, default = 0))
    )
  }
  x <- dplyr::mutate_(x, .dots = lazyeval::as.lazy_dots(args, asNamespace("ggvis")))

  dplyr::ungroup(x)
}

round_fp <- function(x) {
  if (!is.numeric(x)) return(x)
  signif(x, 8)
}

#' @export
compute_stack.ggvis <- function(x, stack_var = NULL, group_var = NULL) {
  args <- list(stack_var = stack_var, group_var = group_var)

  register_computation(x, args, "stack", function(data, args) {
    output <- do_call(compute_stack, quote(data), .args = args)
    preserve_constants(data, output)
  })
}
