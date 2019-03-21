#' Score the NEO Five-Factor Inventory (NEO-FFI) scale (Costa & McCrae, 1992)
#' This is for the 60-item version 
#' 
#' @param df a data.frame containing the 60 NEO items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "NEO"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 5
#' 
#' @details 
#' 
#' Adds five columns, \code{NEO_neurot}, \code{NEO_extra}, \code{NEO_open}, \code{NEO_agree}, and \code{NEO_cons}, to \code{df} containing 
#' the NEO-FFI scales, respectively.
#'
#' Note: the default NEO scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:60
#' will yield the 60 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_neo <- function(df, item_prefix="NEO", max_impute=0.2, 
                       drop_items=FALSE, keep_reverse_codes=FALSE, min_value=1, max_value=5) {
  
  orig_items <- paste0(item_prefix, 1:60) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  reverse_keys <- c(3, 8, 9, 12, 14, 15, 18, 23, 24, 27, 29, 30, 33, 38, 39, 42, 44, 45, 48, 54, 55, 57) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  #define variables and score items. add 'r' suffix to reverse items as needed
  neurot_items <- sapply(seq(1, 56, by=5), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #neuroticism
  extra_items <- sapply(seq(2, 57, by=5), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #extraversion
  open_items <- sapply(seq(3, 58, by=5), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #openness to experience
  agree_items <- sapply(seq(4, 59, by=5), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #agreeableness
  cons_items <- sapply(seq(5, 60, by=5), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #conscientiousness
  
  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-5 scoring by default
  
  
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, neurot_items, thresh=max_impute)
    df <- mean_impute_items(df, extra_items, thresh=max_impute)
    df <- mean_impute_items(df, open_items, thresh=max_impute)
    df <- mean_impute_items(df, agree_items, thresh=max_impute)
    df <- mean_impute_items(df, cons_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    NEO_neurot = rowSums(select(., neurot_items)),
    NEO_extra = rowSums(select(., extra_items)),
    NEO_open = rowSums(select(., open_items)),
    NEO_agree = rowSums(select(., agree_items)),
    NEO_cons = rowSums(select(., cons_items)))
  
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}