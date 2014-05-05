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
    cat(indent(get_data_id(dat), 2), "\n")
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
view_static <- function(x,
                        renderer = getOption("ggvis.renderer", default = "svg"),
                        launch = interactive(),
                        id = rand_id("plot_"),
                        minify = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  temp_dir <- tempfile(pattern = "ggvis")
  dir.create(temp_dir)

  spec <- as.vega(x, dynamic = FALSE)
  vega_json <- RJSONIO::toJSON(spec, pretty = TRUE)

  template <- paste(readLines(system.file('index.html', package='ggvis')),
    collapse='\n')

  head <- html_head(minify = minify)

  # Find the paths of all the JS and CSS files in head tags
  www_paths <- unlist(lapply(head, function(tag) {
    tag$attribs$src %||% tag$attribs$href
  }))
  # Also copy jquery-ui resources
  www_paths <- c(www_paths, "lib/jquery-ui")

  copy_www_resources(www_paths, temp_dir)

  body <- shiny::tagList(
    ggvis_output(id, shiny = FALSE),
    tags$script(type = "text/javascript",
      paste0('
        var ', id, '_spec = ', vega_json, ';
        ggvis.getPlot("', id, '").parseSpec(', id, '_spec);
      ')
    )
  )

  body <- format(body)

  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker::whisker.render(template, list(head = head, body = body)),
    con = html_file)

  if (launch) view_plot(html_file, 350)
  invisible(html_file)
}

copy_www_resources <- function(paths, destdir) {
  # Copies a file/dir from an installed package to the destdir (with path)
  copy_www_path <- function(file, pkg) {
    src <- system.file("www", file, package = "ggvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir, recursive = TRUE)

    if (file.info(src)$isdir) {
      file.copy(src, dirname(destfile), recursive = TRUE)
    } else {
      file.copy(src, destfile, overwrite = TRUE)
    }
  }

  lapply(paths, copy_www_path)
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
view_dynamic <- function(x,
    renderer = getOption("ggvis.renderer", default = "svg"),
    id = rand_id("plot_"), minify = TRUE,
    launch = TRUE, port = NULL, quiet = TRUE) {

  app <- app_object(x, renderer = renderer, id = id, minify = minify)

  if (launch) {
    shiny::runApp(app, port = port, quiet = quiet,
      launch.browser = function(url) view_plot(url, 350 + control_height(x)))
  } else {
    app
  }
}

# Given a ggvis object, return an object that can be run as a Shiny app
app_object <- function(x,
    renderer = getOption("ggvis.renderer", default = "svg"),
    id = rand_id("plot_"), minify = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  # Find number of control elements for the plot
  n_controls <- length(x$controls)

  if (n_controls == 0) {
    ui <- shiny::basicPage(ggvis_output(id, shiny = TRUE, minify = minify))

  } else {
    ui <- sidebarBottomPage(
      sidebarBottomPanel(
        ggvisControlOutput("ggvis_controls", id)
      ),
      mainTopPanel(
        ggvis_output(id, shiny = TRUE)
      )
    )
  }

  server <- function(input, output, session) {
    r_gv <- reactive(x)
    # Set up observers for the spec and the data, and for connecting inputs
    # to reactives.
    observe_ggvis(r_gv, id, session, renderer)

    # User interface elements (in the sidebar). These must be added dynamically
    # (instead of being rendered statically in ui) because they are unique to
    # each session.
    output$ggvis_controls <- renderControls(r_gv, session)
  }

  list(ui = ui, server = server)
}

# View using either an internal viewer or fallback to utils::browseURL
view_plot <- function(url, height) {
  viewer <- getOption("viewer")
  if (!is.null(viewer) && getOption("ggvis.view_internal", TRUE))
    viewer(url, height)
  else
    utils::browseURL(url)
}

# Returns a shiny tagList with links to the needed JS and CSS files
# @param prefix A prefix to add to the path (like "ggvis")
# @param minify Use minified version of JS and CSS files.
# @param shiny Include shiny-related ggvis files?
#' @importFrom shiny tags
html_head <- function(prefix = NULL, minify = TRUE, shiny = FALSE) {
  if(minify) {
    tags <- shiny::tagList(
      # Shiny has its own copy of jQuery; duplicates can cause problems
      if (!shiny) tags$script(src = "lib/jquery/jquery.min.js"),
      tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.4.custom.min.js"),
      tags$script(charset = "utf-8", src = "lib/d3/d3.min.js"),
      tags$script(src = "lib/vega/vega.min.js"),
      tags$script(src = "lib/lodash/lodash.min.js"),
      tags$script("var lodash = _.noConflict();"),
      tags$script(src = "ggvis/js/ggvis.js"),
      if (shiny) tags$script(src = "ggvis/js/shiny-ggvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
        href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.4.custom.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "ggvis/css/ggvis.css")
    )
  } else {
    tags <- shiny::tagList(
      if (!shiny) tags$script(src = "lib/jquery/jquery.js"),
      tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.4.custom.js"),
      tags$script(charset = "utf-8", src = "lib/d3/d3.js"),
      tags$script(src = "lib/vega/vega.js"),
      tags$script(src = "lib/lodash/lodash.min.js"),
      tags$script("var lodash = _.noConflict();"),
      tags$script(src = "ggvis/js/ggvis.js"),
      if (shiny) tags$script(src = "ggvis/js/shiny-ggvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
        href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.4.custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "ggvis/css/ggvis.css")
    )
  }

  if (!is.null(prefix)) {
    tags[] <- lapply(tags, function(tag) {
      if (!is.null(tag$attribs$src)) {
        tag$attribs$src <- file.path(prefix, tag$attribs$src)
      }
      if (!is.null(tag$attribs$href)) {
        tag$attribs$href <- file.path(prefix, tag$attribs$href)
      }
      tag
    })
  }

  tags
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
    ggvis_output(id, shiny = FALSE),
    tags$script(type = "text/javascript",
                paste0('var ', id, '_spec = ', vega_json, ';
                       ggvis.getPlot("', id, '").parseSpec(', id, '_spec);
                       ')
                )
  )

  # Define dependencies
  dependencies <- list(
    html_dependency(name = "jquery",
                    version = "1.11.0",
                    path = "lib/jquery",
                    script = "jquery.min.js"),
    html_dependency(name = "jquery-ui",
                    version = "1.10.4",
                    path = "lib/jquery-ui",
                    script = "js/jquery-ui-1.10.4.custom.min.js",
                    stylesheet = "css/smoothness/jquery-ui-1.10.4.custom.min.css"),
    html_dependency(name = "d3",
                    version = "3.4.1",
                    path = "lib/d3",
                    script = "d3.min.js"),
    html_dependency(name = "vega",
                    version = "1.3.3",
                    path = "lib/vega",
                    script = "vega.min.js"),
    html_dependency(name = "lodash",
                    version = "2.2.1",
                    path = "lib/lodash",
                    script = "lodash.min.js",
                    head = "<script>var lodash = _.noConflict();</script>"),
    html_dependency(name = "ggvis",
                    version = packageVersion("ggvis"),
                    path = "ggvis",
                    script = "js/ggvis.js",
                    stylesheet = "css/ggvis.css")
  )

  # Return knit_asis
  structure(class = "knit_asis",
            format(html, indent = FALSE),
            knit_meta = dependencies
  )
}

html_dependency <- function(name,
                            version,
                            path,
                            meta = NULL,
                            script = NULL,
                            stylesheet = NULL,
                            head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = version,
    path = system.file("www", path, package = "ggvis"),
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}

