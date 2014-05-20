scale_info <- function(prop, data) {
  values <- prop_value(prop, data)

  structure(list(
    label = deparse(prop$value),
    type = vector_type(values),
    domain = data_range(values)
  ), class = "scale_info")
}

# Takes a list of scale_info objects and collapses them into a single scale_info
# object.
collapse_scale_infos <- function(infos) {
  if (empty(infos)) return(NULL)

  domains <- lapply(infos, function(info) info$domain)
  domain <- data_range(unlist(domains, recursive = FALSE))

  structure(list(
    label = infos[[1]]$label,
    type = infos[[1]]$type,
    domain = domain
  ), class = "scale_info")
}

# Takes a named list, where each name is the name of a scale, and each item is
# a list of scale_info objects, and collapses them. The returned object is a
# named list where there's one item per scale, and each item is a reactive that
# returns a scale_info object.
scale_domain_data <- function(scale_info) {
  # scale_info is a named list where name is the name of a scale, and each item
  # is a list of reactives which return scale_info objects.
  domain_data <- lapply(scale_info, function(infos) {
    if (empty(infos)) return(NULL)
    reactive({
      infos_static <- lapply(infos, value)
      data.frame(value = collapse_scale_infos(infos_static)$domain)
    })
  })

  domain_data <- compact(domain_data)

  names(domain_data) <- paste0("domain/", names(domain_data))
  domain_data
}
