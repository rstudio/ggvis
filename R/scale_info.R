#' Create a scale_info object. These objects are typically associated with a
#' mark, and they contain information about the scale. Note that these are
#' different from vega scale objects.
#'
#' @param label Label for the scale.
#' @param type Type of scale.
#' @param domain Can be either a vector of values for the domain, or a reactive
#'   which returns such a vector.
#' @keywords internal
scale_info <- function(label, type, domain) {
  structure(list(
    label = label,
    type = type,
    domain = domain
  ), class = "scale_info")
}

# Takes a list of scale_info objects and collapses them into a single scale_info
# object.
collapse_scale_infos <- function(infos) {
  if (empty(infos)) return(NULL)

  # Get first non-NULL label
  label <- compact(pluck(infos, "label"))[[1]]
  type <- unique(vpluck(infos, "type", character(1)))
  if (length(type) != 1) stop("Scales must all have same type.")

  domains <- pluck(infos, "domain")
  domain <- reactive({
    data_range(concat(values(domains)))
  })

  scale_info(label, type, domain)
}

# scale_info is a named list where name is the name of a scale, and each item
# is a list of reactives which return scale_info objects. This returns a list
# of reactives which return scale info; it essentially collapses each of the
# inner lists.
summarize_scale_infos <- function(scale_infos_list) {
  lapply(scale_infos_list, collapse_scale_infos)
}

# Takes a named list, where each name is the name of a scale, and each item is
# a scale_info object. Returns a named list where there's one item per scale,
# and each item is a reactive that returns a data frame with values for the
# domain.
scale_domain_data <- function(scale_infos_list) {
  scale_infos_list <- compact(scale_infos_list)

  domain_data <- lapply(scale_infos_list, function(info) {
    force(info)
    reactive({
      data.frame(value = value(info$domain))
    })
  })

  names(domain_data) <- paste0("domain/", names(domain_data))
  domain_data
}
