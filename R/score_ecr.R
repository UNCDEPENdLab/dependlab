#' Score the experiences in close relationships -- revised (ECR-R) scale
#' This is for the 36-item version.
#' 
#' @param df a data.frame containing the 36 ECR items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "ECR"
#' @param max_impute the proportion of missingness [0..1) or number [1..] of missing values per scale. Below this, the mean will be imputed for missing items
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param max_value the highest value for the items anchors, used in reverse scoring. Default: 7
#' 
#' @details 
#' 
#' Adds two columns, \code{ECR_anxiety} and \code{ECR_avoidance}, to \code{df} containing
#' the anxiety and avoidance scales, respectively.
#'
#' Note: the default ECR scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:36
#' will yield the 36 items from the test.
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_ecr <- function(df, item_prefix="ECR", max_impute=0.2, 
                      drop_items=FALSE, keep_reverse_codes=FALSE, max_value=7) {
  
  orig_items <- paste0(item_prefix, 1:36) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  reverse_keys <- c(2, 22, 3, 5, 11, 15, 17, 19, 25, 27, 29, 31, 33, 35) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  #even/odd scoring. add 'r' suffix to reverse items as needed
  anx_items <- sapply(seq(2, 36, by=2), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })
  avd_items <- sapply(seq(1, 35, by=2), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })
  
  #add "r" suffix and apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + 1 - x }) #1-7 scoring by default
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, anx_items, thresh=max_impute)
    df <- mean_impute_items(df, avd_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(ECR_anxiety = rowMeans(select(., anx_items)), ECR_avoidance = rowMeans(select(., avd_items)))
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}

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