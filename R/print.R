#' View in a ggvis plot in the browser.
#'
#' \code{view_static} creates a static web page in a temporary directory;
#' \code{view_dynamic} generate a dynamic shiny app and launches it.
#' \code{print} picks between the two methods automatically based on whether
#' or not your plot has interactive features.
#'
#' @param x A ggvis object.
#' @param dynamic Uses \code{view_dynamic} if \code{TRUE}, \code{view_static} if
#'   \code{FALSE}. The default, \code{NA}, chooses automatically based on the
#'   presence of reactives or interactive inputs in \code{x}.
#' @param spec If \code{TRUE}, override usual printing and instead print
#'   the json plot spec. If a character vector, will display just those
#'   components of the spec. This is useful for generating regression tests.
#' @param ... Other arguments passed on to \code{view_dynamic} and
#'   \code{view_static} from \code{print}.
#' @param launch If \code{TRUE}, launch this web page in a browser or Rstudio.
#' @param port the port on which to start the shiny app. If NULL (the default),
#'   Shiny will select a random port.
#' @param quiet If \code{TRUE} show status messages from Shiny. (Default is
#'   \code{FALSE}.)
#' @param minify If \code{TRUE}, use minified version of JS and CSS files. This
#'   can be useful for debugging.
#' @keywords internal
#' @method print ggvis
#' @export
print.ggvis <- function(x, dynamic = NA,
                        spec = getOption("ggvis.print_spec", FALSE),
                        id = rand_id("plot_"), minify = TRUE, ...) {

  set_last_vis(x)

  # Special case for spec printing mode
  if (!identical(spec, FALSE)) {
    return(show_spec(x, spec))
  }

  if (is.na(dynamic)) dynamic <- is.dynamic(x) && interactive()

  if (dynamic) {
    view_dynamic(x, id = id, minify = minify, ...)
  } else {
    view_static(x, id = id, minify = minify, ...)
  }
}

#' Determine if an ggvis is dynamic (i.e. needs to be run in a shiny app)
#'
#' @export
#' @keywords internal
is.dynamic <- function(x) {
  any_apply(x$data, shiny::is.reactive) || length(x$reactives) > 0
}


#' Print out the structure of a ggvis object in a friendly format
#'
#' @param x Visualisation to explain
#' @param ... Needed for compatibility with generic. Ignored by this method.
#' @export
explain.ggvis <- function (x, ...) {
  cat("Marks:\n")
  for (mark in x$marks) {
    cat(indent(format(mark), 2))
  }
  cat("Data objects:\n")
  for (dat in x$data) {
    cat(indent(data_id(dat), 2), "\n")
  }
  cat("Reactives:\n")
  for (reactive in x$reactives) {
    cat(indent(reactive_id(reactive), 2))
    if (is.broker(reactive)) {
      cat(" <Broker>\n")
    } else {
      cat("\n")
    }
  }
  cat("Scales:\n")
  for (scale in x$scales) {
    cat(indent(format(scale), 2))
    cat("\n")
  }
  cat("Axes:\n")
  for (axis in x$axes) {
    cat(indent(format(axis), 2))
    cat("\n")
  }
  cat("Legends:\n")
  for (legend in x$legends) {
    cat(indent(format(legend), 2))
    cat("\n")
  }
  cat("HTML controls:\n")
  for (control_name in names(x$controls)) {
    cat(indent(control_name, 2))
    cat("\n")
  }
  cat("Client-side handlers:\n")
  for (handler in x$handlers) {
    cat(indent(paste0("<", handler$type, "> ", handler$id), 2))
    cat("\n")
  }
  cat("Connector functions:\n")
  for (connector in x$connectors) {
    cat(indent(connector_label(connector), 2))
    cat("\n")
  }
  cat("Options:\n")
  if (length(x$options) > 0) {
    params <- param_string(x$options, collapse = FALSE)
    cat(paste0("  ", format(paste0(names(params), ":")), " ", format(params),
        collapse = "\n"))
    cat("\n")
  }
}

show_spec <- function(x, pieces) {
  out <- as.vega(x, dynamic = FALSE)

  if (is.character(pieces)) {
    out <- out[pieces]
  }

  json <- RJSONIO::toJSON(out, pretty = TRUE)
  cat(gsub("\t", " ", json), "\n", sep = "")

  invisible()
}

#' @rdname print.ggvis
#' @export
view_static <- function(x, plot_id = rand_id("plot_"), minified = TRUE,
                        dest = tempfile(pattern = "ggvis"), launch = TRUE) {

  deps <- ggvis_dependencies(minified = minified)

  if (!file.exists(dest)) dir.create(dest)
  copy_deps(deps, system.file("www", package = "ggvis"), dest)

  spec <- as.vega(x, dynamic = FALSE)
  ui <- ggvis_ui(plot_id, length(x$controls) > 0, spec, deps = deps)

  result <- shiny:::renderTags(ui)
  html_file <- file.path(dest, "plot.html")
  cat(
    '<!DOCTYPE html>\n',
    '<html>\n',
    '<head>\n',
    '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>\n',
    result$head,
    '</head>\n',
    '<body>\n',
    result$html,
    '</body>\n',
    '</html>\n',
    file = html_file, sep = "")

  if (launch) view_plot(html_file, 350)
  invisible(html_file)
}

#' @rdname print.ggvis
#' @export
view_dynamic <- function(x, plot_id = rand_id("plot_"), minified = TRUE,
                         launch = TRUE, port = NULL, quiet = TRUE) {

  deps <- ggvis_dependencies(minified = minified)
  app <- ggvis_app(x, plot_id = plot_id, deps = deps)

  if (launch) {
    shiny::runApp(app, port = port, quiet = quiet,
      launch.browser = function(url) view_plot(url, 350 + control_height(x)))
  } else {
    app
  }
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

#' @rdname print.ggvis
#' @export
knit_print.ggvis <- function(x, options) {

  # Read knitr chunk options for output width and height
  knitr_opts <- list(width = options$out.width.px,
                     height = options$out.height.px)
  x <- add_options(x, knitr_opts, replace = FALSE)

  # if this is a dynamic object, check to see if we're rendering in a Shiny R
  # Markdown document and have an appropriate version of Shiny; emit a Shiny
  # application if we are, and a warning if we aren't.
  if (is.dynamic(x)) {
    if (identical(knitr::opts_knit$get()$rmarkdown.runtime, "shiny") &&
        packageVersion("shiny") >= "0.9.1.9000") {

      # create the application object and allocate space for the controls
      app <- app_object(x)
      knitr_opts$height <- knitr_opts$height + control_height(x)
      return(knitr::knit_print(shiny::shinyApp(ui = app$ui,
                                               server = app$server,
                                               options = knitr_opts)))
    } else {
      warning("Can't output dynamic/interactive ggvis plots in a knitr document.\n",
              "Generating a static (non-dynamic, non-interactive) version of plot.")
    }
  }

  # Plot as JSON
  spec <- as.vega(x, dynamic = FALSE)
  vega_json <- RJSONIO::toJSON(spec, pretty = TRUE)

  # Plot as HTML
  id = rand_id("plot_")
  html <- shiny::tagList(
    ggvisOutput(id, shiny = FALSE),
    tags$script(type = "text/javascript",
                paste0('var ', id, '_spec = ', vega_json, ';
                       ggvis.getPlot("', id, '").parseSpec(', id, '_spec);
                       ')
                )
  )

  # Return knit_asis
  structure(class = "knit_asis",
            format(html, indent = FALSE),
            knit_meta = ggvis_deps
  )
}

