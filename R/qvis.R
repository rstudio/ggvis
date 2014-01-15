#' Quick vis: a succint way of generating simple ggvis plot specifications.
#'
#' \code{qvis} is a succinct way of creating simple plots. It achieves this
#' succinctness by being a little bit magical and trying to guess what you
#' mean. It is most suitable for creating basic data graphics like scatterplots
#' and histograms. As you start creating more complicated plots, you'll
#' want to use \code{\link{ggvis}} directly to get more control and
#' avoid ambiguity.
#'
#' @section Compared to \code{qplot}:
#'
#' \code{qvis} is somewhat less powerful than \code{qplot}. \code{qvis}
#' contains the most commonly used arguments from \code{qplot}, but it
#' drops the many extra arguments that are easy to replicate by adding
#' the right component on to the plot.
#'
#' @param data The data set where
#' @param ... Properties and branch arguments.
#'
#'   If the name of the argument matches the name of a known property, it will
#'   be added to the plot properties. Otherwise it will be passed as an
#'   argument to every branch. If you want to pass different arguments to
#'   different branches, you'll need to add branches on one at a time.
#'
#'   The first two unnamed components are taken to be \code{x} and \code{y}.
#'   Any additional unnamed components will raise an error.
#' @param branches A character vector listing the names of branches to
#'   display on the plot. You can use either the full name of the branch
#'   (e.g. "mark_line" or "branch_smooth"), or just the final part
#'   (e.g. "line" or "smooth"). If there is both a plain mark and a branch
#'   with the same name, the branch will be used.
#'
#'   If \code{branches} is not supplied, it defaults to
#'   \code{"\link{mark_point}"} if both \code{x} and \code{y} are supplied.
#'   If only \code{x} is supplied, it defaults to
#'   \code{"\link{branch_histogram}"}.
#' @export
#' @examples
#' # A basic scatterplot
#' qvis(mtcars, ~mpg, ~wt)
#' qvis(mtcars, ~mpg, ~wt, fill = ~cyl)
#' qvis(mtcars, ~mpg, ~wt, fill := "red")
#'
#' # Basic histogram
#' qvis(mtcars, ~mpg)
#' qvis(mtcars, ~mpg, binwidth = 2)
#'
#' # Scatterplot + smoother
#' qvis(mtcars, ~mpg, ~wt, branches = c("point", "smooth"))
#' qvis(mtcars, ~mpg, ~wt, branches = c("point", "smooth"), span = 0.25)
#'
#' # It's not currently possible to create a plot of variables
#' # stored only in the local environment
#' x <- runif(10)
#' y <- runif(10)
#' \dontrun{qvis(environment(), ~x, ~y)}
qvis <- function(data, ..., branches = character()) {
  args <- dots(...)

  props_args <- qvis_default_names(args[is_props(args)])
  branch_args <- args[!is_props(args)]

  if (length(branches) == 0) {
    if ("y" %in% names(props_args)) {
      branches <- "mark_point"
    } else {
      branches <- "branch_histogram"
    }
  }

  branches <- lapply(branches, init_branch, branch_args)
  ggvis(data, props(.props = props_args)) + branches
}

# TODO: make sure this uses the right scoping so that it will find branches
# defined in other packages and in the global environment.
init_branch <- function(name, args = list()) {
  to_try <- paste0(c("mark_", "branch_", ""), name)

  for (fname in to_try) {
    if (!exists(fname, mode = "function")) next

    f <- get(fname, mode = "function")
    if ("..." %in% names(formals(f))) {
      return(do.call(f, args))
    } else {
      # Don't pass in extra args if doesn't have ... (e.g. all marks)
      return(do.call(f, list()))
    }
  }

  stop("Couldn't find mark or branch called ", name, call. = FALSE)
}

qvis_default_names <- function(args) {
  new_names <- names2(args)

  missing <- new_names == "" & !vapply(args, uses_colon_equals, logical(1))
  n_missing <- sum(missing)

  if (n_missing == 0) return(args)

  if (n_missing == 1) {
    new_names[missing] <- "x"
  } else if (n_missing == 2) {
    new_names[missing] <- c("x", "y")
  } else {
    stop("Only at most two properties can be unnamed", call. = FALSE)
  }

  setNames(args, new_names)
}

# A quoted call is a prop if it's unnamed, or named and in the list of known
# props, or if it's a call to :=
is_props <- function(xs) {
  unnamed <- names2(xs) == ""
  prop_name <- names2(xs) %in% known_props
  uses_colon <- vapply(xs, uses_colon_equals, logical(1))

  unnamed | prop_name | uses_colon
}

# valid_props <- apropos("valid_mark_properties.mark_")
# dput(sort(unique(unlist(lapply(valid_props, function(f) match.fun(f)())))))
known_props <- c("align", "angle", "baseline", "dx", "dy", "endAngle",
  "fill", "fillOpacity", "font", "fontSize", "fontStyle", "fontWeight",
  "height", "innerRadius", "interpolate", "key", "opacity", "outerRadius",
  "shape", "size", "startAngle", "stroke", "strokeOpacity", "strokeWidth",
  "tension", "text", "url", "width", "x", "x2", "y", "y2")
