#' expand.grid-based multivariate apply (mapply) wrapper for fast implementation of nested looping structures
#'
#' @param mvars a named list of variables to be combined and mapped over. These are equivalent to the layers of a nested for loop.
#' @param FUN user-defined function to apply over variables in mvars. N.B. This function is written to use the names of mvars as formal arguments to FUN. Thus, the majority of FUNs need not require any arguments to be specified. As long as the variable object names in FUN match the names of mvars,  gmapply will handle the translation of names(mvars) to FUN.
#' @param SIMPLIFY when set to TRUE, mimics the simplified output of Map to mapply.
#' @param ncores number of cores to utilize if running in parallel. This has not been fully vetted as of yet.
#' @param ...
#'
#' @return compiled returns of FUN
#'
#' @author Nate Hall
#'
#' @examples
#'
#' \dontrun{
#'      # Example 1:
#'      # just make sure variables used in your function appear as the names of mvars
#'      myfunc <- function(...){
#'        return_me <- paste(l3, l1^2 + l2, sep = "_")
#'        return(return_me)
#'      }
#'
#'      mvars <- list(l1 = 1:10,
#'                    l2 = 1:5,
#'                    l3 = letters[1:3])
#'
#'      gmapply(mvars, myfunc) # list output (mapply)
#'      lreturns <- gmapply(mvars, myfunc, SIMPLIFY = TRUE) # concatenated output (Map)
#'
#'      # N.B. This is equivalent to running:
#'      lreturns <- c()
#'      for(l1 in 1:10){
#'        for(l2 in 1:5){
#'          for(l3 in letters[1:3]){
#'            lreturns <- c(lreturns,myfunc(l1,l2,l3))
#'          }
#'        }
#'      }
#'
#'
#'
#' }
#'
#' @importFrom parallel mcmapply
#'
#' @export

gmapply <- function(mvars, FUN, SIMPLIFY = FALSE, ncores = NULL, ...){

  FUN <- match.fun(FUN)
  expand.dots <- list(...) # allows for expanded dot args to be passed as formal args to the user specified function

  ## build grid of mvars to loop over, this ensures that each combination of various inputs is evaluated (equivalent to creating a structure of nested for loops)
  grid <- expand.grid(mvars,KEEP.OUT.ATTRS = FALSE)

  # specify formals of the function to be evaluated  by merging grid to mapply over and expanded dot args
  argdefs <- rep(list(bquote()), ncol(grid) + length(expand.dots) + 1)
  names(argdefs) <- c(colnames(grid), names(expand.dots), "...")

  argdefs[which(names(argdefs) %in% names(expand.dots))] <- expand.dots # replace with proper dot arg inputs.

  formals(FUN) <- argdefs

  if(is.null(ncores)){ # serial
    if(SIMPLIFY) {
      #standard mapply
      do.call(mapply, c(FUN, c(unname(grid))))
    } else{
      #standard Map
      do.call(mapply, c(FUN, c(unname(grid), SIMPLIFY = FALSE)))
    }
  } else{ # parallel. NOT FULLY VETTED.
    if(SIMPLIFY) {
      #standard mapply
      do.call(mcmapply, c(FUN, c(unname(grid), mc.cores = ncores)))
    } else{
      #standard Map
      do.call(mcmapply, c(FUN, c(unname(grid), SIMPLIFY = FALSE, mc.cores = ncores)))
    }
  }



}
