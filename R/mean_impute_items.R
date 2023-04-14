#' Function to impute the mean for missing items on a per-case (row) basis
#' if the proportion of missingness falls below a specified threshold (e.g. 0.2)
#' 
#' @param scale_df a data.frame object whose columns are the items to be imputed
#' @param scale_items a character vector of items names the define the scale/subscale to be imputed
#' @param thresh the proportion of missingness [0..1) or number [1..] of missing values below which the mean will be imputed for missing items
#'  
#' @details 
#' 
#' If the proportion of missingness is greater than \code{thresh} then no items will be imputed
#' and the observation will not be altered.
#' 
#' @author Michael Hallquist
#' @export
#' 
mean_impute_items <- function(scale_df, scale_items=NULL, thresh=0.2) {
  stopifnot(is.data.frame(scale_df)) #validate input
  if (is.null(scale_items)) { stop("You must specify which items are part of the scale.") }
  
  #assumes that all columns passed to this function are part of the scale/subscale
  mean_na_thresh <- function(vec, thresh=0.2) {
    is_na <- is.na(vec)
    na_test <- sum(is_na)/ifelse(thresh < 1, length(vec), 1) #if thresh is > 1, check the number, not proportion
    if (na_test > 0 && na_test <= thresh) { 
      m_nonmiss = mean(vec, na.rm=TRUE) 
      vec[is_na] <- m_nonmiss
    }
    return(vec)
  }
  
  scale_df[,scale_items] <- t(apply(scale_df[,scale_items], 1, mean_na_thresh, thresh=thresh))
  return(scale_df)
}