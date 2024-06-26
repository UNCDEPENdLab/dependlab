#' @title Useful functions for the UNC Chapel Hill DEPENd Lab (Director: Michael Hallquist)
#' @name dependlab-package
#' @aliases dependlab
#'
#' @description This package provides handy R functions for common tasks in the lab including
#'   obtaining predictions from multilevel models, computing targeted correlations, or building
#'   design matrices for model-based fMRI analyses.
#'
#' @details At the moment, the package provides 3 core utilities: lmer_predict, cor_with_target, and build_design_matrix.
#'   More will be added over time.
#'
#' \tabular{ll}{
#' Package: \tab dependlab\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1-1\cr
#' Date: \tab 2018-11-05\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' LazyData: \tab yes\cr
#' }
#'
#' @author
#' Michael Hallquist \email{michael.hallquist@@gmail.com},
#' Alison Schreiber \email{almasc526@@gmail.com}
#'
#' Maintainer: Michael Hallquist \email{michael.hallquist@@gmail.com}
#' @keywords package
#' @useDynLib dependlab, .registration = TRUE
#' @import lme4
#' @importFrom utils packageDescription
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
