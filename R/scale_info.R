#' Create a scale_info object. These objects are typically associated with a
#' mark, and they contain information about the scale. Note that these are
#' different from vega scale objects.
#'
#' @param label Label for the scale.
#' @param type Type of scale.
#' @param domain Can be either a vector of values for the domain, or a reactive
#'   which returns such a vector.
#' @param override Should the domain specified by this scale_info object
#'   override other scale_infos for the same scale? Useful when domain is
#'   manually specified.
#' @keywords internal
scale_info <- function(label, type, domain, override = FALSE) {
  structure(list(
    label = label,
    type = type,
    domain = domain,
    override = override
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
  overrides <- vpluck(infos, "override", logical(1))

  # Set the domain based on whether there is an override domain, and the type
  # of domain.
  if (any(overrides)) {
    # Use the last overriding domain, if more than one
    over <- domains[[last(which(overrides))]]
    under <- domains[!overrides]

    if (countable_prop_type(type)) {
      # For categorical props, just use the override domain
      domain <- reactive(values(over))

    } else {
      # For continuous props, if either of the values in the override domain
      # is NA, then use the upper/lower bound of the non-override domains for
      # that value.
      domain <- reactive({
        over_vals <- value(over)

        if (anyNA(over_vals)) {
          under_vals <- data_range(concat(values(under)))
          if (is.na(over_vals[1])) over_vals[1] <- under_vals[1]
          if (is.na(over_vals[2])) over_vals[2] <- under_vals[2]
        }
        over_vals
      })
    }

  } else {
    # If no overrides, just get range from the data
    domain <- reactive({
      data_range(concat(values(domains)))
    })
  }

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
