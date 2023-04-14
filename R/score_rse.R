#' Score the Relationship and Sex Education (RSE) scale
#' 
#' @param df a data.frame containing the 10 RSE items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "RSE"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 4
#' 
#' @details 
#' 
#' Adds one column, \code{RSE_total}, to \code{df} containing the sum of scores.
#'
#' Note: the default RSE scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:10
#' will yield the 10 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_rse <- function(df, item_prefix="RSE", max_impute=0.2, 
                        drop_items=FALSE, keep_reverse_codes=FALSE, min_value=1, max_value=4) {
  
  orig_items <- paste0(item_prefix, 1:10) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  reverse_keys <- c(2, 5, 6, 8, 9) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  
  #score items. add 'r' sufficx to reverse items as needed
  total_items <- sapply(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })
  
  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-4 scoring by default
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, total_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    RSE_total = rowSums(select(., total_items)))
  
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}