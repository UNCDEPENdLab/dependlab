#' Score the Positive and Negative Affect Schedule (PANAS) scale 
#' 
#' @param df a data.frame containing the 20 PANAS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "PANAS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors. Default: 1
#' @param max_value the highest value for the item anchors. Default: 5
#' 
#' @details 
#' 
#' Adds two columns, \code{PANAS_positive} and \code{PANAS_negative}, to \code{df} containing
#' the positive and negative affect scales, respectively.
#'
#' Note: the default PANAS scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:20
#' will yield the 20 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_panas <- function(df, item_prefix="PANAS", max_impute=0.2, 
                      drop_items=FALSE, min_value=1, max_value=5) {
  
  orig_items <- paste0(item_prefix, 1:20) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  #positive and negative affect scales
  pos_items <- paste0(item_prefix, c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)) #positive affect
  neg_items <- paste0(item_prefix, c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)) #negative affect
  
  #NB. There is no reverse scoring for the PANAS
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, pos_items, thresh=max_impute)
    df <- mean_impute_items(df, neg_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    PANAS_pos = rowSums(select(., pos_items)),
    PANAS_neg = rowSums(select(., neg_items)))
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  
  return(df)
}