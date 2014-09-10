#' Cocaine seizures in the US.
#'
#' This dataset comes from STRIDE, the System to Retrieve Information from Drug
#' Evidence. It contains all concaine seizures in the US from 2007 that have
#' a known weight.
#'
#' @section Variables:
#' \describe{
#'   \item{state}{State where seizure occured.}
#'   \item{potency}{Purity of cocaine, as percentage (100\% = pure cocaine,
#'     0\% = all filler)}
#'   \item{weight}{Weight, in grams.}
#'   \item{month}{Month in which seizure occured.}
#'   \item{price}{Estimated value in USD.}
#' }
#' @section Use:
#' Use of this data requires your agreement to refer to your analyses as
#' "unvalidated DEA data and to claim authorship and responsibility for any
#' inferences and/or conclusions you may draw from this information."
#' @format Data frame with 3380 observations of 5 variables.
#' @source
#' \url{http://www.justice.gov/dea/resource-center/stride-data.shtml}
"cocaine"
