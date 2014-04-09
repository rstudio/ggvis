#' Transformation: sort the data
#'
#' \code{transform_sort} is a data transformation that sorts a data object based
#' on one or more columns in the data.
#'
#' @section Input:
#' \code{transform_sort} takes a data frame or a split_df as input.
#'
#' @section Ouput:
#'
#' \code{transform_sort} returns a sorted data frame or split_df with the same
#'   columns as the input. In the case of a split_df, each of the data frames
#'   contained within is sorted.
#'
#' @param var The variables to sort on. This is the variable name after mapping.
#'   For example, with \code{props(x = ~mpg)}, you would use \code{"x"}, not
#'   \code{"mpg"}. Multiple variables can be used, as in \code{c("x", "y")}.
#' @param ... Named arguments, which are passed along to the \code{order()}
#'   function used for sorting. Unnamed arguments will be dropped.
#' @examples
#'
#' # Compare the output from the unsorted vs. sorted data
#' # Unsorted
#' ggvis(mtcars,
#'   props(x = ~wt, y = ~mpg),
#'   layer_path(),
#'   layer_point(props(fill := NA))
#' )
#'
#' # Sorted
#' ggvis(mtcars,
#'   transform_sort(),
#'   props(x = ~wt, y = ~mpg),
#'   layer_path(),
#'   layer_point(props(fill := NA))
#' )
#'
#' # With grouping
#' ggvis(mtcars,
#'   by_group(factor(cyl)),
#'   transform_sort(),
#'   props(x = ~wt, y = ~mpg, stroke = ~factor(cyl)),
#'   layer_path(),
#'   layer_point(props(fill := NA))
#' )
#'
#' # Sort on mpg column
#' sluice(pipeline(mtcars, transform_sort()), props(x = ~mpg))
#' # Same effect, but this time mpg is mapped to y
#' sluice(pipeline(mtcars, transform_sort(var = "y")), props(y = ~mpg))
#'
#' # Sort on multiple columns
#' sluice(pipeline(mtcars, transform_sort(var = c("x", "y"))),
#'   props(x = ~cyl, y = ~mpg))
#'
#' # Use `decreasing` argument, which passed along to order()
#' sluice(pipeline(mtcars, transform_sort(var = "x", decreasing = TRUE)),
#'   props(x = ~mpg))
#'
#' # Sort on a calculated column (mpg mod 10)
#' sluice(pipeline(mtcars, transform_sort()), props(x = ~mpg %% 10) )
#'
#' @export
transform_sort <- function(vis, ..., vars = "x") {
  if (!is.ggvis(vis)) stop("First argument to transform must be a ggvis object.")

  dots <- list(...)
  dots <- dots[named(dots)]

  # Get the data and props from the parent
  parent_data_id <- vis$cur_data_id
  parent_props_id <- vis$cur_props_id

  parent_data <- vis$data[[parent_data_id]]
  parent_props <- vis$props[[parent_props_id]]


  # Create the data for the current node
  new_data <- reactive({
    prop_names <- paste0(vars, ".update")
    vapply(prop_names,
      function(prop_name) check_prop(x, parent_props, data, prop_name),
      logical(1)
    )

    output <- compute_sort(data, vars = parent_props[prop_names], dots = dots)
    preserve_constants(data, output)
  })

  # Set the id for the current data - hash the options to this transform
  data_id <- paste0(parent_data_id, "_transform_sort_",
                    digest(list(dots, vars), algo = "crc32"))

  # Save data and current data_id in the vis
  vis$data[[data_id]] <- new_data
  vis$cur_data_id <- data_id

  vis
}

#' @export
format.transform_sort <- function(x, ...) {
  paste0(" -> sort()", param_string(x["var"]))
}

compute_sort <- function(data, vars, dots) UseMethod("compute_sort")

#' @export
compute_sort.split_df <- function(data, vars, dots) {
  data[] <- lapply(data, compute_sort, vars = vars, dots = dots)
  data
}

#' @export
compute_sort.data.frame <- function(data, vars, dots) {
  cols <- lapply(vars, prop_value, data)
  idx <- do.call(order, args = c(cols, dots))
  data[idx, ]
}
