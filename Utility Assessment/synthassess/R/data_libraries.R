#' @title U.S. Public Libraries Data - Real and Synthetic
#' @rdname libraries
#' @description A real dataset and a synthetic version of the same dataset.
#' Data are from a stratified sample of public libraries in the United States in FY2020 (April 2020 to March 2021).
#' This is a sample of 217 libraries taken
#' from an annual census of public libraries in the U.S. \cr \cr
#' @examples
#' # Load datasets
#' data(libraries_orig)
#' data(libraries_synth)
#' 
#' # Analyze data using provided weights
#' library(survey)
#' library(srvyr)
#' 
#' libraries_orig_svy <- as_survey_rep(
#'   .data      = libraries_orig,
#'   weights    = FULL_SAMPLE_WGT,
#'   repweights = num_range("REP_WGT_", 1:80),
#'   type       = "successive-difference", 
#'   mse        = TRUE
#' )
#' libraries_synth_svy <- as_survey_rep(
#'   .data      = libraries_synth,
#'   weights    = FULL_SAMPLE_WGT,
#'   repweights = num_range("REP_WGT_", 1:80),
#'   type       = "successive-difference", 
#'   mse        = TRUE
#' )
#' @usage data(libraries_orig)
#' @seealso See [svrep::library_stsys_sample] for details on the original data source.
#' @format
#' Both datasets contain the same number of records and the same set of variables.
#' These variables include:
#' \cr \cr
#' Unique identifier:
#' \itemize{
#'   \item FSCSKEY: A unique identifier for libraries.
#' }
#' Numeric summaries:
#' \itemize{
#'   \item LIBRARIA: Total librarians (measured in full-time equivalent staff)
#'   \item TOTSTAFF: Total staff (measured in full-time equivalent staff)
#'   \item TOTCIR: Total circulation
#'   \item TOTOPEXP: Total operating expenses
#'   \item TOTINCM: Total income
#' }
#' Location:
#' \itemize{
#'   \item STABR: Two-letter state abbreviation
#'
#' }
#' Weights:
#' \itemize{
#'   \item FULL_SAMPLE_WGT: Full-sample weight to use for point estimates
#'   \item REP_WGT_1 - REP_WGT_80: Eighty replicate weights, created using the successive-differences replication method.
#' }
"libraries_orig"

#' @rdname libraries
#' @usage data(libraries_synth)
"libraries_synth"