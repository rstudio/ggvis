#' Creates a named list, giving the properties used by each dataset.
#'
#' @noRd
#' @examples
#' base <- mtcars %>% ggvis(~wt, ~mpg)
#' base %>% layer_points() %>% combine_data_props()
#' base %>% layer_points() %>% layer_points() %>% combine_data_props()
#' base %>% layer_points(y = ~cyl) %>% layer_points(y = ~disp) %>% combine_data_props()
#' base %>% layer_points() %>% layer_points(~conc, ~uptake, data = CO2) %>%
#'  combine_data_props()
combine_data_props <- function(mark) {
  if (is.ggvis(mark)) return(combine_data_props(mark$marks))
  if (identical(class(mark), "mark")) {
    return(setNames(list(mark$props), data_id(mark$data)))
  }

  if (is.mark_group(mark)) {
    children <- mark$marks
  } else if (is.list(mark)) {
    children <- mark
  } else {
    stop("Invalid input")
  }
  all_props <- unlist(lapply(children, combine_data_props), recursive = FALSE)

  # Combine together props for data with same name
  props_by_id <- split(all_props, names(all_props))
  props_by_id <- lapply(props_by_id, unlist, recursive = FALSE, use.names = FALSE)

  # Remove duplicates, and props that don't appear in the data
  lapply(props_by_id, function(props) {
    names <- safe_vega_var(vapply(props, prop_label, character(1)))
    ok <- !duplicated(names) & names != ""

    setNames(props[ok], names[ok])
  })
}

# Create a new reactive dataset containing only the data actually used
# by properties.
active_props <- function(data, props) {
  reactive_prop <- function(props, parent_data) {
    force(props)
    force(parent_data)
    reactive({
      apply_props(parent_data(), props)
    })
  }

  data_out <- list()
  for (data_n in names(props)) {
    data_out[[data_n]] <- reactive_prop(props[[data_n]], data[[data_n]])
  }

  data_out
}

# Apply properties to a data object, creating calculated columns and dropping
# unused columns.
apply_props <- function(data, props) {
  UseMethod("apply_props")
}

#' @export
apply_props.data.frame <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  names(cols) <- vapply(props, prop_label, character(1))
  quickdf(cols)
}

#' @export
apply_props.grouped_df <- function(data, props) {
  dplyr::do(data, apply_props(., props))
}
