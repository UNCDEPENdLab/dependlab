savePkgList <- function () 
{
  if (!file.exists("~/rpackages.RData")) {
    warning("cannot find r package")
    pkgs <- c()
  }
  else {
    load("~/rpackages.RData")
  }
  newpkgs <- installed.packages()[, "Package"]
  pkgs <- union(pkgs, newpkgs)
  save(pkgs, file = "~/rpackages.RData")
}
