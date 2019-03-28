#' Installs packages that are missing based off a list provided. Usually, this list comes from a previous save of the R environment. 
#' This is for the 32-item version
#' 
#' @details 
#' 
#' Note: this function is useful when updating R, as it allows you to re-install packages that were lost when transitioning from an older version to a newer version. 
#' will yield the 32 items from the test.
#' 
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 


installMissingPkgs <- function () 
{
  if (!file.exists("~/rpackages.RData")) {
    stop("cannot find r package")
  }
  load("~/rpackages.RData")
  installed <- pkgs %in% installed.packages()[, "Package"]
  if (length(pkgs[!installed]) >= 1) {
    install.packages(pkgs[!installed])
  }
}
