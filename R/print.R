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

  if (getOption("knitr.in.progress", FALSE)) {
    return(knitr_print(x, dynamic, id = id, minify = minify, ...))
  }

  if (is.na(dynamic)) dynamic <- is.dynamic(x) && interactive()

  if (dynamic) {
    view_dynamic(x, id = id, minify = minify, ...)
  } else {
    view_static(x, id = id, minify = minify, ...)
  }
}

show_spec <- function(x, pieces) {
  out <- as.vega(x, dynamic = FALSE)

  if (is.character(pieces)) {
    out <- out[pieces]
  }

  json <- toJSON(out, pretty = TRUE)
  cat(gsub("\t", " ", json), "\n", sep = "")

  invisible()
}

#' @rdname print.ggvis
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
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
  vega_json <- toJSON(spec, pretty = TRUE)

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

  body <- tagList(
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
  writeLines(whisker.render(template, list(head = head, body = body)),
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
      file.copy(src, destfile)
    }
  }

  lapply(paths, copy_www_path)
}

#' @rdname print.ggvis
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
#' @importFrom shiny basicPage uiOutput mainPanel tags observe runApp stopApp renderUI
view_dynamic <- function(x,
    renderer = getOption("ggvis.renderer", default = "svg"),
    id = rand_id("plot_"), minify = TRUE,
    launch = TRUE, port = NULL, quiet = TRUE) {

  app <- app_object(x, renderer = renderer, id = id, minify = minify)

  if (launch) {
    # Find number of control elements for the plot
    n_controls <- length(controls(x))

    # Request 70 vertical pixels for each pair of control items, since there are
    # two on a row.
    height <- 350 + 70 * ceiling(n_controls / 2)

    shiny::runApp(app, port = port, quiet = quiet,
      launch.browser = function(url) view_plot(url, height))
  } else {
    app
  }
}

# Given a ggvis object, return an object that can be run as a Shiny app
app_object <- function(x, renderer, id, minify) {
  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  # Find number of control elements for the plot
  n_controls <- length(controls(x))

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
    # Set up observers for the spec and the data
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

# Print from within a knitr document.
# The knitr chunk must use the results="asis" option for this to work properly
knitr_print <- function(x, dynamic = NA, id = rand_id("plot_"), minify = TRUE,
                        ...) {

  # Read knitr chunk options (if present) for default values
  x$opts <- list(merge_opts(knitr_opts(), x$opts[[1]]))

  if (is.na(dynamic)) dynamic <- is.dynamic(x)
  if (dynamic) {
    warning("Can't output dynamic/interactive ggvis plots in a knitr document.\n",
      "Generating a static (non-dynamic, non-interactive) version of plot.")
  }

  spec <- as.vega(x, dynamic = FALSE)
  vega_json <- toJSON(spec, pretty = TRUE)

  body <- tagList(
    ggvis_output(id, shiny = FALSE, minify = minify),
    tags$script(type = "text/javascript",
      paste0('var ', id, '_spec = ', vega_json, ';
        ggvis.getPlot("', id, '").parseSpec(', id, '_spec);
      ')
    )
  )
  cat(format(body, indent = FALSE))
}

# Returns a shiny tagList with links to the needed JS and CSS files
# @param prefix A prefix to add to the path (like "ggvis")
# @param minify Use minified version of JS and CSS files.
# @param shiny Include shiny-related ggvis files?
html_head <- function(prefix = NULL, minify = TRUE, shiny = FALSE) {
  if(minify) {
    tags <- tagList(
      # Shiny has its own copy of jQuery; duplicates can cause problems
      if (!shiny) tags$script(src = "lib/jquery-1.11.0.min.js"),
      tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.4.custom.min.js"),
      tags$script(charset = "utf-8", src = "lib/d3.min.js"),
      tags$script(src = "lib/vega.min.js"),
      tags$script(src = "lib/QuadTree.js"),
      tags$script(src = "lib/lodash.min.js"),
      tags$script("var lodash = _.noConflict();"),
      tags$script(src = "js/ggvis.js"),
      if (shiny) tags$script(src = "js/shiny-ggvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
        href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.4.custom.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/ggvis.css")
    )
  } else {
    tags <- tagList(
      if (!shiny) tags$script(src = "lib/jquery-1.11.0.js"),
      tags$script(src = "lib/jquery-ui/js/jquery-ui-1.10.4.custom.js"),
      tags$script(charset = "utf-8", src = "lib/d3.js"),
      tags$script(src = "lib/vega.js"),
      tags$script(src = "lib/QuadTree.js"),
      tags$script(src = "lib/lodash.min.js"),
      tags$script("var lodash = _.noConflict();"),
      tags$script(src = "js/ggvis.js"),
      if (shiny) tags$script(src = "js/shiny-ggvis.js"),
      tags$link(rel = "stylesheet", type = "text/css",
        href = "lib/jquery-ui/css/smoothness/jquery-ui-1.10.4.custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/ggvis.css")
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
