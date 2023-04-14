#' Score the Interpersonal Stressors Circumplex (ISC) 
#' 
#' @param df a data.frame containing the 64 ISC items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "ISC"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors. Default: 1
#' @param max_value the highest value for the item anchors. Default: 8
#'
#' @details 
#' 
#' Adds eight columns, \code{ISC_c1}, \code{ISC_c2}, \code{ISC_c3}, \code{ISC_c4}, \code{ISC_c5}, \code{ISC_c6}, \code{ISC_c7}, and \code{ISC_c8}, to \code{df} containing
#' the different subscales, respectively.
#'
#' Note: the default ISC scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:64
#' will yield the 64 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_isc <- function(df, item_prefix="ISC", max_impute=0.2, 
                      drop_items=FALSE, min_value=1, max_value=8) {
  
  orig_items <- paste0(item_prefix, 1:64) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  #define variables and score items.
  c1_items <- paste0(item_prefix, c(1, 9, 17, 25, 33, 41, 49, 57)) #sensitive to control
  c2_items <- paste0(item_prefix, c(2, 10, 18, 26, 34, 42, 50, 58)) #sensitive to antagonism
  c3_items <- paste0(item_prefix, c(3, 11, 19, 27, 35, 43, 51, 59)) #sensitive to remoteness
  c4_items <- paste0(item_prefix, c(4, 12, 20, 28, 35, 44, 52, 60)) #sensitive to timidity
  c5_items <- paste0(item_prefix, c(5, 13, 21, 29, 37, 45, 53, 61)) #sensitive to passivity 
  c6_items <- paste0(item_prefix, c(6, 14, 22, 30, 38, 46, 54, 62)) #sensitive to dependence
  c7_items <- paste0(item_prefix, c(7, 15, 23, 31, 39, 47, 55, 63)) #sensitive to affection
  c8_items <- paste0(item_prefix, c(8, 16, 24, 32, 40, 48, 56, 64)) #sensitive to attention-seeking
  
  #NB. There is no reverse scoring for the ISC
  
  #mean impute
  if (max_impute > 0) {
    df <- mean_impute_items(df, c1_items, thresh=max_impute)
    df <- mean_impute_items(df, c2_items, thresh=max_impute)
    df <- mean_impute_items(df, c3_items, thresh=max_impute)
    df <- mean_impute_items(df, c4_items, thresh=max_impute)
    df <- mean_impute_items(df, c5_items, thresh=max_impute)
    df <- mean_impute_items(df, c6_items, thresh=max_impute)
    df <- mean_impute_items(df, c7_items, thresh=max_impute)
    df <- mean_impute_items(df, c8_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    ISC_c1 = rowMeans(select(., c1_items)),
    ISC_c2 = rowMeans(select(., c2_items)),
    ISC_c3 = rowMeans(select(., c3_items)),
    ISC_c4 = rowMeans(select(., c4_items)),
    ISC_c5 = rowMeans(select(., c5_items)),
    ISC_c6 = rowMeans(select(., c6_items)),
    ISC_c7 = rowMeans(select(., c7_items)),
    ISC_c8 = rowMeans(select(., c8_items)))
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  
  
  return(df)
}