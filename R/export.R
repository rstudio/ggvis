#' Export a PNG or SVG from a ggvis object
#'
#' This requires that the external program \code{vg2png} is installed. This is
#' part of the \code{vega} node.js module.
#'
#' @seealso \url{https://github.com/trifacta/vega} for information on installing
#'   \code{vg2png} and \code{vg2svg}.
#'
#' @param vis A ggvis object.
#' @param file Output file name. If NULL, defaults to "plot.svg" or "plot.png".
#' @examples
#' \dontrun{
#' mtcars %>% ggvis(x = ~wt) %>% export_png()
#' }
#' @export
export_png <- function(vis, file = NULL) {
  vega_file(vis, file = file, type = "png")
}

#' @rdname export_png
#' @export
export_svg <- function(vis, file = NULL) {
  vega_file(vis, file = file, type = "svg")
}

# Generate an output image file from a ggvis object
#
# @param vis A ggvis object.
# @param file Output file name. If NULL, defaults to "plot.png".
# @param type Output file type.
vega_file <- function(vis, file = NULL, type = "png") {

  if (!(type %in% c("png", "svg")))  stop("type must be 'png' or 'svg'")

  if (is.null(file)) {
    file <- paste0("plot.", type)
    message("Writing to file ", file)
  }

  temp_dir <- tempfile(pattern = "ggvis")
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
  json_file <- file.path(temp_dir, "plot.json")
  vega_json <- save_spec(vis, json_file)
  on.exit(unlink(json_file))

  # Create the image file
  system2(cmd, args = c(json_file, file))
}
