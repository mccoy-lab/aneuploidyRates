#' aneuploidy_rates: A package for computing the meiotic and mitotic error rates
#' of human embryos.
#'
#'
#' Run find_rates to generate the error rate data.
#'
#' @section This package includes five main functions:
#' create_embryo, prob_to_prop, take_biopsy, summarize_biopsy, and find_rates
#'
#' summarize_biopsy uses create_embryo, prob_to_prop, and take_biopsy.
#'
#' find_rates uses summarize_biopsy.
#'
#' @docType package
#' @name aneuploidy_rates
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import tessera
#' @importFrom EasyABC ABC_rejection
#' @importFrom methods new
#' @importFrom stats rnorm runif
#' @importFrom utils head
## usethis namespace: end
NULL

