# There are five way to display a ggvis plot:
#   * static html file
#   * temporary shiny app
#   * static html embedded in Rmarkdown
#   * shiny app embedded in Rmarkdown
#   * embedded in regular shiny app


#' View in a ggvis plot in the browser.
#'
#' \code{view_static} creates a static web page in a temporary directory;
#' \code{view_dynamic} generate a dynamic shiny app and launches it.
#' \code{print} automatically picks between the two.
#'
#' @param x A ggvis object.
#' @param dynamic Uses \code{view_dynamic} if \code{TRUE}, \code{view_static} if
#'   \code{FALSE}. The default, \code{NA}, chooses automatically based on the
#'   presence of reactives or interactive inputs in \code{x}.
#' @param launch If \code{TRUE}, will launch plot in a viewer/browser. If
#'   \code{FALSE} returns an object that you can \code{print()} to launch.
#' @param ... Other arguments passed on to \code{view_dynamic} and
#'   \code{view_static} ?from \code{print}.
#' @keywords internal
#' @method print ggvis
#' @export
print.ggvis <- function(x, dynamic = NA, launch = interactive(), ...) {
  if (is.na(dynamic)) {
    dynamic <- is.dynamic(x) && interactive()
  }

  if (dynamic) {
    out <- view_dynamic(x, ...)
  } else {
    out <- view_static(x, ...)
  }

  if (launch) print(out)
  out
}

#' @rdname print.ggvis
#' @export
#' @param plot_id Unique identifier used to identify the plot on the page.
#' @param dest Directory in which to save html and depedencies. Created if
#'   it doesn't already exist.
view_static <- function(x, plot_id = rand_id("plot_"),
                        dest = tempfile(pattern = "ggvis")) {

  spec <- as.vega(x, dynamic = FALSE)
  htmltools::browsable(ggvisLayout(plot_id, length(x$controls) > 0, spec))
}

#' @rdname print.ggvis
#' @export
#' @param port the port on which to start the shiny app. If NULL (the default),
#'   Shiny will select a random port.
#' @param quiet If \code{TRUE} show status messages from Shiny. (Default is
#'   \code{FALSE}.)
view_dynamic <- function(x, plot_id = rand_id("plot_"), port = NULL,
                         quiet = FALSE) {

  ggvis_app(x,
    plot_id = plot_id,
    port = port,
    quiet = TRUE,
    launch.browser = function(url) {
      if (!quiet) {
        message("Showing dynamic visualisation. Press Escape/Ctrl + C to stop.")
      }
      view_plot(url, 350 + control_height(x))
    }
  )
}

#' Knit print method for ggvis plots.
#'
#' @keywords internal
#' @export
knit_print.ggvis <- function(x, options = list(), inline = FALSE, ...) {
  # Set height and width from knitr chunk options
  knitr_opts <- list(
    width = options$out.width.px,
    height = options$out.height.px
  )
  x <- add_options(x, knitr_opts, replace = FALSE)

  # If this is a dynamic object, check to see if we're rendering in a Shiny R
  # Markdown document and have an appropriate version of Shiny; emit a Shiny
  # application if we are, and a warning if we aren't.
  if (is.dynamic(x)) {
    if (identical(runtime(), "shiny")) {
      # create the application object and allocate space for the controls
      app <- ggvis_app(x,
        width = knitr_opts$width,
        height = knitr_opts$height + control_height(x)
      )
      return(knitr::knit_print(app))
    }

    warning(
      "Can't output dynamic/interactive ggvis plots in a knitr document.\n",
      "Generating a static (non-dynamic, non-interactive) version of the plot.",
      call. = FALSE
    )
  }

  spec <- as.vega(x, dynamic = FALSE)
  knit_print(ggvisOutput(spec = spec))
}

# Helper functions -------------------------------------------------------------

runtime <- function() {
  knitr::opts_knit$get("rmarkdown.runtime")
}

#' Determine if an ggvis is dynamic (i.e. needs to be run in a shiny app)
#'
#' @export
#' @keywords internal
is.dynamic <- function(x) {
  any_apply(x$data, shiny::is.reactive) || length(x$reactives) > 0
}

#' @export
print.showUrl <- function(x, ...) {
  view_plot(x, 350)
}

# View using either an internal viewer or fallback to utils::browseURL
view_plot <- function(url, height) {
  viewer <- getOption("viewer")
  if (!is.null(viewer) && getOption("ggvis.view_internal", TRUE)) {
    viewer(url, height)
  } else {
    utils::browseURL(url)
  }
}

# given a ggvis object, return the number of pixels to reserve for its controls.
control_height <- function(x) {
  n_controls <- length(x$controls)

  # Request 70 vertical pixels for each pair of control items, since there are
  # two on a row.
  70 * ceiling(n_controls / 2)
}
