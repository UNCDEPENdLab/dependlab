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