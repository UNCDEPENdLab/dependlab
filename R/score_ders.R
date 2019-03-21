#' Score the Difficulties in Emotion Regulation Scale (DERS) (Gratz & Roemer, 2004)
#' 
#' @param df a data.frame containing the 36 DERS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "DERS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 5
#' 
#' @details 
#' 
#' Adds seven columns, \code{DERS_nonaccept}, \code{DERS_goals}, \code{DERS_impulse}, \code{DERS_aware}, \code{DERS_strategies}, \code{DERS_clarity}, and \code{DERS_total}, to \code{df} containing
#' the different subscales, respectively.
#'
#' Note: the default DERS scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:36
#' will yield the 36 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_ders <- function(df, item_prefix="DERS", max_impute=0.2, 
                      drop_items=FALSE, keep_reverse_codes=FALSE, min_value=1, max_value=5) {
  
  orig_items <- paste0(item_prefix, 1:36) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  reverse_keys <- c(1, 2, 6, 7, 8, 10, 17, 20, 22, 24, 34) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  
  #define variables and score items. add 'r' suffix to reverse items as needed
  nonaccept_items <- sapply(c(11, 12, 21, 23, 25, 29), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #non-acceptance of emotional responses (NONACCEPT)
  goals_items <- sapply(c(13, 18, 20, 26, 33), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #difficulties engaging in goal-directed behaviour (GOALS)
  impulse_items <- sapply(c(3, 14, 19, 24, 27, 32), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #impulse control difficulties (IMPULSE)
  aware_items <- sapply(c(2, 6, 8, 10, 17, 34), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #lack of emotional awareness (AWARE)
  strategies_items <- sapply(c(15, 16, 22, 28, 30, 31, 35, 36), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #limited access to emotion regulation strategies (STRATEGIES)
  clarity_items <- sapply(c(1, 4, 5, 7, 9), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #lack of emotional clarity (CLARITY)
  total_items <- sapply(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #total score (SUM)
  
  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-5 scoring by default
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, nonaccept_items, thresh=max_impute)
    df <- mean_impute_items(df, goals_items, thresh=max_impute)
    df <- mean_impute_items(df, impulse_items, thresh=max_impute)
    df <- mean_impute_items(df, aware_items, thresh=max_impute)
    df <- mean_impute_items(df, strategies_items, thresh=max_impute)
    df <- mean_impute_items(df, clarity_items, thresh=max_impute)
    df <- mean_impute_items(df, total_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    DERS_nonaccept = rowSums(select(., nonaccept_items)),
    DERS_goals = rowSums(select(., goals_items)),
    DERS_impulse = rowSums(select(., impulse_items)),
    DERS_aware = rowSums(select(., aware_items)),
    DERS_strategies = rowSums(select(., strategies_items)),
    DERS_clarity = rowSums(select(., clarity_items)),
    DERS_total = rowSums(select(., total_items)))

  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}
