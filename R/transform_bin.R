#' Transformation: bin continuous variable.
#'
#' \code{transform_bin} is a data transformation that reduces a one-d vector
#' of positions to a data frame of binned counts. \code{layer_histogram}
#' combines \code{transform_bin} with \code{mark_rect} to create a histogram,
#' and \code{layer_freqpoly} combines \code{transform_bin} with
#' \code{mark_path} to create a frequency polygon.
#'
#' @section Input:
#' Currently \code{transform_bin} only works with data frames and requires the
#' following properties:
#'
#' \itemize{
#'   \item \code{x}, numeric, the values to bin and count
#' }
#'
#'
#' @param vis Visualisation to modify
#' @param ... For \code{transform_bin}: ignored, all transforms must use
#'   named arguments.  For \code{layer_histogram}: named arguments are
#'   passed on to the transform, unnamed arguments are passed on to the layer.
#' @export
#' @examples
#' # Create histograms and frequency polygons with layers
#' ggvis(mtcars, props(x = ~mpg), layer_histogram())
#' ggvis(mtcars, props(x = ~mpg), layer_histogram(binwidth = 2))
#' ggvis(mtcars, props(x = ~mpg), layer_freqpoly(binwidth = 2))
#'
#' # These are equivalent to combining transform_bin with the corresponding
#' # mark
#' ggvis(mtcars, props(x = ~mpg), transform_bin(binwidth = 2),
#'   mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0)),
#'   mark_path(props(x = ~x, y = ~count__, stroke := "red", strokeWidth := 4))
#' )
#'
#' # You can also combine other data transformations like splitting
#' ggvis(mtcars, props(x = ~mpg, stroke = ~cyl, strokeWidth := 4),
#'    by_group(cyl), layer_freqpoly(binwidth = 2))
#'
#' # You can see the results of a transformation by creating your own pipeline
#' # and flowing data through it
#' sluice(pipeline(mtcars, transform_bin(2)), props(x = ~mpg))
#' sluice(pipeline(mtcars, by_group(cyl), transform_bin(2)), props(x = ~disp))
#' # Or
#' pl <- pipeline(mtcars, transform_bin(10))
#' sluice(pl, props(x = ~disp))
transform_bin <- function(vis, ..., binwidth = guess(), origin = NULL,
                          right = TRUE) {
  if (!is.ggvis(vis)) stop("First argument to transform must be a ggvis object.")

  dots <- list(...)
  dots <- dots[named(dots)]

  # Get the current data and props from the parent
  parent_data <- vis$cur_data
  parent_props <- vis$cur_props

  # Register reactive arguments
  vis <- register_reactives(vis, c(dots, binwidth, origin, right))

  new_data <- reactive({
    data <- parent_data()

    check_prop("transform_bin", parent_props, data, "x.update",
      c("numeric", "datetime", "ordinal", "nominal"))

    range <- prop_range(data, parent_props$x)
    params <- bin_params(value(range), binwidth = value(binwidth),
                         origin = value(origin), right = value(right))

    output <- compute_bin(data, x_var = parent_props$x,
                          binwidth = params$binwidth, origin = params$origin,
                          right = params$right)

    # TODO: Check for zero-row output for other data types
    if (is.data.frame(output) && nrow(output) == 0) return(output)

    preserve_constants(data, output)
  })

  # Save data in the vis object, updating current data.
  register_data(vis,
    new_data,
    prefix = paste0(get_data_id(parent_data), "_transform_sort"),
    update_current = TRUE
  )
}

#' @rdname transform_bin
#' @export
layer_histogram <- function(vis, ...) {
  vis %>%
    branch(
      transform_bin(...) %>%
      mark_rect(props(x = ~xmin__, x2 = ~xmax__, y = ~count__, y2 = 0))
    )
}

#' @rdname transform_bin
#' @export
#' @inheritParams layer_histogram
layer_freqpoly <- function(vis, ...) {
  vis %>%
    branch(
      transform_bin(...) %>%
      mark_path(props(x = ~x, y = ~count__))
    )
}

#' @rdname transform_bin
#' @export
layer_barchart <- function(vis, ...) {
  vis %>%
    branch(
      transform_bin(...) %>%
      mark_rect(props(x = ~x, width = band(mult = 0.9), y2 = ~count__, y = 0))
    )
}
