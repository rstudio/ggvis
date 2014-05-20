# Takes a list of the registered ranges for each scale and collapses them so
# that there's one range per scale. Returns a list of reactives, where each
# reactive returns a data frame.
scale_domain_data <- function(scale_domains) {
  domain_data <- lapply(scale_domains, function(scale_domain) {
    if (length(scale_domain) == 0) return(NULL)
    reactive({
      # Get the range of all the ranges together
      domains <- lapply(scale_domain, function(prop_domain) prop_domain())
      domain <- unlist(domains, recursive = FALSE)
      ## TODO: Get prop type and attach as attribute?
      data.frame(value = data_range(domain))
    })
  })

  domain_data <- compact(domain_data)

  names(domain_data) <- paste0("domain/", names(domain_data))
  as.environment(domain_data)
}
