#' rat.brain data in Chapter 5 of West, Welch, and Galecki regression book
#'
#' The data used in this example were originally reported by Douglas, et al. (2004).
#' The aim of their experiment was to examine nucleotide activation (guanine nucleotide bonding)
#' in seven different brain nuclei (i.e., brain regions) among five adult male rats. 
#' 
#' @format A data frame with 30 observations on the following 4 variables.
#' \describe{
#'   \item{animal}{Unique identifier for each rat}
#'   \item{treatment}{Level of drug treatment: Basal or Carbachol}
#'   \item{region}{Brain nucleus: BST, LS, or VDB}
#'   \item{activate}{Nucleotide activation (the dependent variable)}
#' }
#' @source \url{https://cran.r-project.org/web/packages/WWGbook/index.html}
"rat.brain"
