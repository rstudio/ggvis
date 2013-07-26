#' Given a filled gigvis spec object, output a gigvis_prerender object
#'
#' @export
gigvis_prerender <- function(gv,
                      width = 600, height = 400, padding = c(20, 20, 30, 50),
                      envir = parent.frame()) {

  # Create a table of the data reactives used in the gigvis object.
  data_table <- register_data_reactives(gv, table = new.env())

  structure(
    list(
      vega_spec = vega_spec(gv, data_table),
      data_table = data_table
    ),
    class = "gigvis_prerender"
  )
}


# Register data reactives at this node and its descendents in a table
# Returns the table (which is an environment)
register_data_reactives <- function(node, table = new.env()) {
  # If this node's data_id isn't already in the table, add an entry for the
  # data object
  if (!is.null(node$data_id) &&
      !exists(node$data_id, where = table, inherits = FALSE)) {

    assign(node$data_id, node$data, envir = table)
  }

  # Recurse into children
  lapply(node$children, register_data_reactives, table = table)

  table
}
