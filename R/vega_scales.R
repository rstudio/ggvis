# Given a gigvis object, get the scales
gather_scales <- function(node, datasets) {
  all_mappings <- gather_mappings(node)

  vega_scales <- list()
  for (scale in unique(all_mappings$scale)) {
    # Get all the mappings for this scale
    mappings <- all_mappings[all_mappings$scale == scale, ]

    vega_scales[[scale]] <- vega_scale(node$scale[[scale]], mappings$var,
      mappings$data)
  }
  unname(vega_scales)
}


# Recursively find all mappings at this node and below, and return a data
# frame of them.
gather_mappings <- function(node) {
  # Find child node mappings and put them in a data frame
  mappings <- lapply(node$children, gather_mappings)
  mappings <- do.call(rbind, mappings)

  # Find the mappings for this node, returning data frame with columns
  # scale, data, var
  if (!is.null(node$data) && !is.null(node$mapping)) {
    mapping <- data.frame(row.names = NULL, stringsAsFactors = FALSE,
      scale = names(node$mapping),
      data  = node$data,
      var   = node$mapping
    )

    # Add current node's mapping to data frame
    mappings <- rbind(mappings, mapping)
  }

  # Drop duplicate rows
  unique(mappings)
}


# Given a gigvis scale, domain (the name of a source column, like 'mpg'), and
# name of data set, return a vega scale specification. Domain and data can be
# vectors with length > 1, though they must have the same length.
vega_scale <- function(scale, domain, data) {
  if (length(domain) != length(data))
    stop("domain and data must be the same length.")

  if (length(domain) == 1) {
    domain_list <- list(data = data, field = paste0("data.", domain))

  } else {
    # If more than one thing mapped to this scale, wrap them in another list
    domain_list <- mapply(domain, data, SIMPLIFY = FALSE,
      FUN = function(domain_, data_) {
        list(data = data_, field = paste0("data.", domain_))
    })
    domain_list <- list(fields = unname(domain_list))
  }

  if (scale$name == "x") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "width",
      domain = domain_list,
      zero   = FALSE,
      nice   = FALSE
    )

  } else if (scale$name == "y") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "height",
      domain = domain_list,
      zero   = FALSE,
      nice   = FALSE
    )

  } else if (scale$name == "color") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "category10"
    )

  } else if (scale$name == "fill") {
    list(
      name   = scale$name,
      type   = scale$type,
      range  = "category10"
    )

  } else {
    stop("Unknown scale: ", scale$name)
  }
}
