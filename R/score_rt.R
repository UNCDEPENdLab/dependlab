#' Score the Risk Taking -- 18 item scale (RT-18) 
#' 
#' @param df a data.frame containing the 18 RT items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "RT"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param input_codes the original T/F coding scheme of the input dataframe. These are converted to the specified min and max value.
#' @param min_value the minimum value for the item anchors. Default: 0
#' @param max_value the highest value for the item anchors. Default: 1
#' 
#' @details 
#' 
#' Adds three columns, \code{RT_riskbehavior}, \code{RT_riskassessment}, and \code{RT_total}, to \code{df} containing
#' the different subscales, respectively.
#'
#' Note: the default RT scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:18
#' will yield the 18 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_rt <- function(df, item_prefix="RT", max_impute=0.2, 
                        drop_items=FALSE, input_codes=c(1,2), min_value=0, max_value=1) {
  
  orig_items <- paste0(item_prefix, 1:18) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #convert values
  df[,orig_items] <-  data.frame(ifelse(df[,orig_items] == input_codes[1], max_value,min_value))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  #define variables and score items
  RB_items <- paste0(item_prefix, c(1, 2, 3, 4, 5, 6, 7, 8, 9)) #risk-taking behavior
  RA_items <- paste0(item_prefix, c(10, 11, 12, 13, 14, 15, 16, 17, 18)) #risk assessment
  total_items <- paste0(item_prefix, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
  
  #NB. There is no reverse scoring for the RT-18
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, RB_items, thresh=max_impute)
    df <- mean_impute_items(df, RA_items, thresh=max_impute)
    df <- mean_impute_items(df, total_items, thres=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    RT_RB = rowSums(select(., RB_items)),
    RT_RA = rowSums(select(., RA_items)),
    RT_total = rowSums(select(., total_items)))
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  
  return(df)
}