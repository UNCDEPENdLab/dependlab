#' Score the Childhood Trauma Questionnaire -- CTQ scale (Bernstein & Fink, 1998)
#' 
#' @param df a data.frame containing the 28 CTQ items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "CTQ"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 5
#' 
#' @details 
#' 
#' Adds six columns, \code{CTQ_emo_abuse}, \code{CTQ_phys_abuse}, \code{CTQ_sex_abuse}, \code{CTQ_emo_neglect}, \code{CTQ_phys_neglect}, and \code{CTQ_denial},  to \code{df} containing
#' the different subscales, respectively.
#'
#' Note: the default CTQ scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:28
#' will yield the 28 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_ctq <- function(df, item_prefix="CTQ", max_impute=0.2, 
                       drop_items=FALSE, keep_reverse_codes=FALSE, min_value=1, max_value=5) {
  
  orig_items <- paste0(item_prefix, 1:28) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  reverse_keys <- c(2, 5, 7, 13, 19, 26, 28) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  
  #define variables and score items. add 'r' suffix to reverse items as needed
  emo_abuse_items <- sapply(c(3, 8, 14, 18, 25), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #emotional abuse
  phys_abuse_items <- sapply(c(9, 11, 12, 15, 17), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #physical abuse
  sex_abuse_items <- sapply(c(20, 21, 23, 24, 27), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #sexual abuse
  emo_neglect_items <- sapply(c(5, 7, 13, 19, 28), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #emotional neglect
  phys_neglect_items <- sapply(c(1, 2, 4, 6, 26), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #physical neglect
  total_items <- sapply(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27, 28), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #scale total score
  denial_items <- sapply(c(10, 16, 22), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #minimization/denial
  
  #NB. CTQ scoring manual states for the 3-item denial scale, 1 point is given for each item with a score of 5 and 0 points for items with score less than 5.
  df[,denial_items] <- ifelse(df[,denial_items] ==5, 1, 0)

  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-5 scoring by default
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, emo_abuse_items, thresh=max_impute)
    df <- mean_impute_items(df, phys_abuse_items, thresh=max_impute)
    df <- mean_impute_items(df, sex_abuse_items, thresh=max_impute)
    df <- mean_impute_items(df, emo_neglect_items, thresh=max_impute)
    df <- mean_impute_items(df, phys_neglect_items, thresh=max_impute)
    df <- mean_impute_items(df, denial_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    CTQ_emo_abuse = rowSums(select(., emo_abuse_items)),
    CTQ_phys_abuse = rowSums(select(., phys_abuse_items)),
    CTQ_sex_abuse = rowSums(select(., sex_abuse_items)),
    CTQ_emo_neglect = rowSums(select(., emo_neglect_items)),
    CTQ_phys_neglect = rowSums(select(., phys_neglect_items)),
    CTQ_total = rowSums(select(., total_items)),
    CTQ_denial = rowSums(select(., denial_items)))
  
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}