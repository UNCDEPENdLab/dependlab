#' Render a pretty AIC comparison table in flextable for iterative model building and manuscripts.
#'
#' @param minfo named character vector containing model infomation to print to the table. (e.g. fixed and random effects)
#' @param mlist list of object models to include in table
#'
#'
#' @return publication-ready table of AIC comparisons in flextable format.
#' @author Michael Hallquist
#' @author Nate Hall
#' @importFrom bbmle AICtab
#' @importFrom flextable flextable autofit as_paragraph
#' @export


render_flex_aictab <- function(minfo, ..., mlist = NULL) {
  # browser()
  if(is.null(mlist)){ # original implementation, pass in multiple models as dot args.
    mc <- match.call(expand.dots = TRUE)
    mlist <- list(...)

    #N.B. AICtab fails if you try to assign names to the list names(mlist) <- ...
    mnames <- sapply(3:length(mc), function(x) { as.character(mc[[x]])})
    mdf <- do.call(rbind, minfo) %>% as.data.frame() %>% rownames_to_column("model")
    adf <- AICtab(mlist, sort=FALSE, delta=TRUE, weights=TRUE, logLik=TRUE, base=TRUE, mnames=mnames) %>%
      as.data.frame() %>% rownames_to_column("model") %>% dplyr::select(-dLogLik)
    res <- mdf %>% left_join(adf, by="model")
  } else {
    ## assumes that input mlist matches minfo
    mnames <- names(mlist)
    mdf <- do.call(rbind, minfo) %>% as.data.frame() %>% rownames_to_column("model")
    adf <- AICtab(mlist, sort=FALSE, delta=TRUE, weights=TRUE, logLik=TRUE, base=TRUE, mnames=mnames) %>%
      as.data.frame() %>% rownames_to_column("model") %>% dplyr::select(-dLogLik)
    res <- mdf %>% left_join(adf, by="model")
  }

  x <- res%>% mutate(weight = round(weight,3))
  colnames(x)[which(colnames(x) == "dAIC")] <- paste0(as_paragraph("\U1D6AB")[[1]][1], "AIC") # add unicode delta to be fancy.


  flexout <- flextable(x) %>% autofit()

  return(flexout)
}


#' Render Variance Inflation Factor (VIF) in flextable
#'
#' @param m model object
#'
#' @return VIF of predictors in model.
#' @author Michael Hallquist
#' @importFrom flextable flextable autofit
#' @export

flex_vif <- function(m) {
  flextable(data.frame(vif=car::vif(m)) %>% rownames_to_column("Predictor")) %>% autofit()
}

#' Probe singular fits of random effects in mixed effects regression models
#'
#' @param m lmer model object
#'
#' @return Concerning RE estimates in m.
#' @author Michael Hallquist
#' @export


check_singularity <- function(lmerobj) {
  tt <- getME(lmerobj,"theta") #RE estimates
  ll <- getME(lmerobj,"lower") #lower bound on estimate

  #look for RE estimates that are very small (near zero) and the lower bound encompasses 0
  low_re <- tt[ll==0 & tt < 1e-2]
  return(low_re)

  #eventually, would be nice to provide functionality to drop these from model and refit automatically.
  # Second that ^
}


