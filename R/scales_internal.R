# Internal-facing versions of scale_xxx functions.
# These have the same interface as the user-facing functions, but don't validate
# arguments or set any defaults.

scale_numeric_int <- function(vis, property, domain = NULL, range = NULL,
                              reverse = NULL, round = NULL,
                              trans = NULL, clamp = NULL, exponent = NULL,
                              nice = NULL, zero = NULL, name = NULL,
                              label = NULL) {
  name <- name %||% property
  label <- label %||% name

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = trans,
    subclass = "numeric",
    exponent = exponent,
    clamp = clamp,
    nice = nice,
    zero = zero,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )
  add_scale(vis, vscale)
}

scale_datetime_int <- function(vis, property, domain = NULL, range = NULL,
                               reverse = NULL, round = NULL, utc = NULL,
                               clamp = NULL, nice = NULL, name = NULL,
                               label = NULL) {
  name <- name %||% property
  label <- label %||% name
  if (!is.null(utc)) {
    utc <- if (isTRUE(utc)) "utc" else "time"
  }

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = utc,
    subclass = "datetime",
    clamp = clamp,
    nice = nice,
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )
  add_scale(vis, vscale)
}

scale_ordinal_int <- function(vis, property, domain = NULL, range = NULL,
                              reverse = FALSE, round = FALSE,
                              points = TRUE, padding = NULL, sort = FALSE,
                              name = NULL, label = NULL) {
  name <- name %||% property
  label <- label %||% name

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = "ordinal",
    points = points,
    padding = padding,
    sort = sort,
    subclass = "ordinal",
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )
  add_scale(vis, vscale)
}

scale_nominal_int <- function(vis, property, domain = NULL, range = NULL,
                              reverse = FALSE, round = FALSE,
                              points = TRUE, padding = NULL, sort = FALSE,
                              name = NULL, label = NULL) {
  name <- name %||% property
  label <- label %||% name

  vscale <- ggvis_scale(
    property = property,
    name = name,
    label = label,
    type = "ordinal",
    points = points,
    padding = padding,
    sort = sort,
    subclass = "nominal",
    domain = domain,
    range = range,
    reverse = reverse,
    round = round
  )

  add_scale(vis, vscale)
}
