# Helper functions for lmer model building and comparison -----------------
## written by MNH with minor tweaks from NTH

#' Render a pretty AIC comparison table in flextable for iterative model building and manuscrips.
#'
#' @param minfo named character vector containing model infomation to print to the table. (e.g. fixed and random effects)
#' @param mobs list of object models to include in table
#'
#'
#' @return A time series of the same length containing reconstructed neural events
#' @author Michael Hallquist
#' @author Nate Hall
#' @importFrom bbmle AICtab
#' @export


render_aictab <- function(minfo, mobs =NULL, ...) {
  mc <- match.call(expand.dots = TRUE)
  mlist <- list(...)

  #N.B. AICtab fails if you try to assign names to the list names(mlist) <- ...
  mnames <- sapply(3:length(mc), function(x) { as.character(mc[[x]])})
  mdf <- do.call(rbind, minfo) %>% as.data.frame() %>% rownames_to_column("model")
  adf <- AICtab(mlist, sort=FALSE, delta=TRUE, weights=TRUE, logLik=TRUE, base=TRUE, mnames=mnames) %>%
    as.data.frame() %>% rownames_to_column("model") %>% dplyr::select(-dLogLik)
  res <- mdf %>% left_join(adf, by="model")
  return(res)
}


# implement in a sec.
#
# flextb_vif <- function(m) {
#   flextable(data.frame(vif=car::vif(m)) %>% rownames_to_column("Predictor")) %>% autofit()
# }
#
# check_singularity <- function(lmerobj) {
#   tt <- getME(lmerobj,"theta") #RE estimates
#   ll <- getME(lmerobj,"lower") #lower bound on estimate
#
#   #look for RE estimates that are very small (near zero) and the lower bound encompasses 0
#   low_re <- tt[ll==0 & tt < 1e-2]
#   return(low_re)
#
#   #eventually, would be nice to provide functionality to drop these from model and refit automatically
# }
