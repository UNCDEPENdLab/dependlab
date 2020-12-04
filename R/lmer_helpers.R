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




