#' Generate a static web page with the embedded gigvis graph
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param renderer The renderer to use in the browser. Can be \code{"canvas"}
#'   (the default) or \code{"svg"}.
#' @param launch If \code{TRUE}, launch this web page in a browser.
#'
#' @export
#' @importFrom RJSONIO toJSON
#' @importFrom whisker whisker.render
view_static <- function(gv, envir = parent.frame(), renderer = "canvas",
                        launch = TRUE) {

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  temp_dir <- tempfile(pattern = "gigvis")
  dir.create(temp_dir)

  copy_www_resources(temp_dir)

  vega_json <- toJSON(vega_spec(gv, envir = envir), pretty = TRUE)

  template <- paste(readLines(system.file('index.html', package='gigvis')),
    collapse='\n')

  js <- paste0(
    '<script type="text/javascript">
      function parse(spec) {
        vg.parse.spec(spec, function(chart) {
          chart({el:"#vis", renderer: "', renderer, '"}).update();
        });
      }
      parse(', vega_json, ');
    </script>')

  body <- paste('<div id="vis"></div>', js, sep ='\n')

  html_file <- file.path(temp_dir, "plot.html")
  writeLines(whisker.render(template, list(head = '', body = body)),
    con = html_file)

  browseURL(html_file)
}

# This is used similarly to view_static, but view_dynamic can take functions
# or (reactive expressions) as data instead of names in an environment.
view_dynamic <- function(gv, envir = parent.frame(), controls = NULL,
                         renderer = "canvas", launch = TRUE, port = 8228) {

  if (!require("shiny"))
    stop("Can't proceed without shiny package")

  if (!(renderer %in% c("canvas", "svg")))
    stop("renderer must be 'canvas' or 'svg'")

  plot_id <- "plot1"

  dynamic_spec <- vega_spec(gv, envir = envir, dynamic = TRUE)
  datasets <- attr(dynamic_spec, "datasets")
  dynamic_spec_json <- RJSONIO::toJSON(dynamic_spec, pretty = TRUE)

  # Make our resources available
  script_tags <- deploy_www_resources()
  if (is.null(controls)) {
    ui <- basicPage(
      tags$head(script_tags),
      gigvisOutput2(plot_id, dynamic_spec_json, renderer = renderer)
    )
  } else {
    ui <- bootstrapPage(
      tags$head(script_tags),
      tags$div(
        class = "container",
        tags$div(
          class = "row",
          tags$div(
            class = "span3",
            tags$div(
              class = "well",
              controls
            )
          ),
          tags$div(
            class = "span9",
            gigvisOutput2(plot_id, dynamic_spec_json, renderer = renderer)
          )
        )
      )
    )
  }

  server <- function(input, output, session) {
    for (name in names(datasets)) {
      # The datasets list contains named objects. The names are synthetic IDs
      # that are present in the vega spec. The values can be a variety of things,
      # see the if/else clauses below.
      local({
        # Have to do everything in a local so that these variables are not shared
        # between the different iterations

        data_name <- name
        obs <- observe({
          data <- get_data_dynamic(datasets[[data_name]], envir = envir)

          session$sendCustomMessage("gigvis_data", list(
            plot = plot_id,
            name = data_name,
            value = d3df(data)
          ))
        })
        session$onSessionEnded(function() {
          obs$suspend()
        })
      })
    }
  }

  runApp(list(ui=ui, server=server))
}

gigvisOutput2 <- function(outputId, vega_json, renderer = 'canvas') {
  js <- paste0(
  '<script type="text/javascript">
    var spec = ', vega_json, ';
    vg.parse.spec(spec, function(chart) {
      var chart = chart({el:"#', outputId, '", renderer: "', renderer, '"});
      $("#', outputId, '").data("gigvis-chart", chart);
      gigvisInit("', outputId, '");
    });
  </script>')
  list(HTML(js), tags$div(id=outputId))
}


#' Generate an PNG file from a gigvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2png}.
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.png".
#' @export
vega_png <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "png")
}

#' Generate a SVG file from a gigvis object
#'
#' This requires that the program \code{vg2png} is installed. This is part of
#' \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2svg}.
#'
#' @param gv A gigvis object.
#' @param envir The environment in which to evaluate \code{gv}.
#' @param file Output file name. If NULL, defaults to "plot.svg".
#' @export
vega_svg <- function(gv, envir = parent.frame(), file = NULL) {
  vega_file(gv, envir, file = file, type = "svg")
}

# Generate an output image file from a gigvis object
#
# @param gv A gigvis object.
# @param envir The environment in which to evaluate \code{gv}.
# @param file Output file name. If NULL, defaults to "plot.png".
# @param type Output file type.
vega_file <- function(gv, envir = parent.frame(), file = NULL,
                      type = "png") {

  if (!(type %in% c("png", "svg")))  stop("type must be 'png' or 'svg'")

  if (is.null(file))  file <- paste0("plot.", type)

  temp_dir <- tempfile(pattern = "gigvis")
  dir.create(temp_dir)

  # Try to find the external program that generates the images
  cmd <- paste0("vg2", type)

  # Search in these paths and use the first one found
  cmdsearch <- Sys.which(paste0(c("", "./bin/", "./node_modules/.bin/"), cmd))
  found_idx <- which(nzchar(cmdsearch))
  if (length(found_idx) == 0)
    stop("Conversion program ", cmd, "not found.")
  cmd <- cmdsearch[min(found_idx)]

  # Generate the Vega JSON spec
  vega_json <- toJSON(vega_spec(gv, envir = envir), pretty = TRUE)
  json_file <- file.path(temp_dir, "plot.json")
  cat(vega_json, file = json_file)
  on.exit(unlink(json_file))

  # Create the image file
  system2(cmd, args = c(json_file, file))
}


copy_www_resources <- function(destdir) {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js"
  )

  lapply(files, function(file) {
    src <- system.file("www", file, package = "gigvis")

    destfile <- file.path(destdir, file)
    parent_dir <- dirname(destfile)
    if (!dir.exists(parent_dir))
      dir.create(parent_dir)

    file.copy(src, destfile)
  })
}

deploy_www_resources <- function() {
  files <- c(
    "lib/jquery-1.9.1.js",
    "lib/d3.js",
    "lib/vega.js",
    "lib/QuadTree.js",
    "js/shiny-gigvis.js"
  )

  addResourcePath("gigvis", system.file("www", package="gigvis"))
  lapply(files, function(file) {
    tags$script(src=paste("gigvis", file, sep="/"))
  })
}