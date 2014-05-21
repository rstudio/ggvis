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
    label = vapply(infos, function(info) info$label, character(1)),
    type = infos[[1]]$type,
    domain = domain
  ), class = "scale_info")
}


# Given a list of reactives which return scale_info objects, return a reactive
# which collapses those scale_info objects into a single one.
collapse_scale_infos_r <- function(infos_r) {
  if (empty(infos_r)) return(NULL)

  reactive({
    infos_static <- lapply(infos_r, function(x) x())
    collapse_scale_infos(infos_static)
  })
}

# scale_info is a named list where name is the name of a scale, and each item
# is a list of reactives which return scale_info objects. This returns a list
# of reactives which return scale info; it essentially collapses each of the
# inner lists.
summarize_scale_infos_r <- function(scale_info_r_list) {
  lapply(scale_info_r_list, collapse_scale_infos_r)
}

# Takes a named list, where each name is the name of a scale, and each item is
# a reactive which returns a scale_ifo object. Returns a named list where
# there's one item per scale, and each item is a reactive that returns a data
# frame with values for the domain.
scale_domain_data <- function(scale_info_r_list) {
  scale_info_r_list <- compact(scale_info_r_list)

  domain_data <- lapply(scale_info_r_list, function(infos_r) {
    force(infos_r)
    reactive({
      data.frame(value = infos_r()$domain)
    })
  })

  names(domain_data) <- paste0("domain/", names(domain_data))
  domain_data
}
