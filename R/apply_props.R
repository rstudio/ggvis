# Apply properties to a data object, creating calculated columns and dropping
# unused columns.
apply_props <- function(data, props) UseMethod("apply_props")

#' @S3method apply_props data.frame
apply_props.data.frame <- function(data, props) {
  cols <- lapply(props, prop_value, data = data)
  names(cols) <- vapply(props, prop_name, character(1))

  as.data.frame(compact(cols))
}

#' @S3method apply_props split_df
apply_props.split_df <- function(data, props) {
  split_df_apply(data, apply_props, props)
}
