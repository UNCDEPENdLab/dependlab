#' Score the Conflict Tactics Scale (CTS)
#'
#' @param df a data.frame containing the 40 CTS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "CTS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per subscale.
#'            Below this, the mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the lowest value for the items anchors, used to check response validity. Default: 1
#' @param max_value the highest value for the items anchors, used to check response validity. Default: 7
#'
#' @details
#'
#' UPDATE ME!
#' Adds two columns, \code{ECR_anxiety} and \code{ECR_avoidance}, to \code{df} containing
#' the anxiety and avoidance scales, respectively.
#'
#' Note: the default CTS scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:40
#' will yield the 40 items from the test.
#'
#' @export
#' @author Michael Hallquist
#'
#' @importFrom dplyr select mutate
#'
score_cts <- function(df, item_prefix="CTS", max_impute=0.2,
                        drop_items=FALSE, min_value=1, max_value=7) {
  
  # warning("This function is not complete yet. Just returning original data.frame for now.")
  # return(df)
  
  orig_items <- paste0(item_prefix, 1:40) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  

  #DEFINITIONS: v=victim, p=perpetrator, m=minor, s=severe
  PsychAggV_items <- paste0(item_prefix, c(1, 11, 15, 19, 27, 33, 35, 37)) 
  PsychAggP_items <- paste0(item_prefix, c(2, 12, 16, 20, 28, 34, 36, 38)) 
  PhysAssV_items <- paste0(item_prefix, c(3, 5, 7, 9, 13, 17, 21, 23, 25, 29, 31, 39)) 
  PhysAssP_items <- paste0(item_prefix, c(4, 6, 8, 10, 14, 18, 22, 24, 26, 30, 32, 40)) 
  PsychAggVM_items <- paste0(item_prefix, c(1, 19, 27, 35)) 
  PsychAggPM_items <- paste0(item_prefix, c(2, 20, 28, 36)) 
  PsychAggVS_items <- paste0(item_prefix, c(11, 15, 33, 37))
  PsychAggPS_items <- paste0(item_prefix, c(12, 16, 34, 38))
  PhysAssVM_items <- paste0(item_prefix, c(3, 5, 7, 25, 29))
  PhysAssPM_items <- paste0(item_prefix, c(4, 6, 8, 26, 30))
  PhysAssVS_items <- paste0(item_prefix, c(9, 13, 17, 21, 23, 31, 39))
  PhysAssPS_items <- paste0(item_prefix, c(10, 14, 18, 22, 24, 32, 40))
  Victim_items <- paste0(item_prefix, c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39))
  Perp_items <- paste0(item_prefix, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40))
  
  #NB. There is no reverse scoring for the CTS
  
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, PsychAggV_items, thresh=max_impute)
    df <- mean_impute_items(df, PsychAggP_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssV_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssP_items, thresh=max_impute)
    df <- mean_impute_items(df, PsychAggVM_items, thresh=max_impute)
    df <- mean_impute_items(df, PsychAggPM_items, thresh=max_impute)
    df <- mean_impute_items(df, PsychAggVS_items, thresh=max_impute)
    df <- mean_impute_items(df, PsychAggPS_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssVM_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssPM_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssVS_items, thresh=max_impute)
    df <- mean_impute_items(df, PhysAssPS_items, thresh=max_impute)
    df <- mean_impute_items(df, Victim_items, thresh=max_impute)
    df <- mean_impute_items(df, Perp_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    CTS_PsychAggV = rowMeans(select(., PsychAggV_items)),
    CTS_PsychAggP = rowMeans(select(., PsychAggP_items)),
    CTS_PhysAssV = rowMeans(select(., PhysAssV_items)),
    CTS_PhysAssP = rowMeans(select(., PhysAssP_items)),
    CTS_PsychAggVM = rowMeans(select(., PsychAggVM_items)),
    CTS_PsychAggPM = rowMeans(select(., PsychAggPM_items)),
    CTS_PsychAggVS = rowMeans(select(., PsychAggVS_items)),
    CTS_PsychAggPS = rowMeans(select(., PsychAggPS_items)),
    CTS_PhysAssVM = rowMeans(select(., PhysAssVM_items)),
    CTS_PhysAssPM = rowMeans(select(., PhysAssPM_items)),
    CTS_PhysAssVS = rowMeans(select(., PhysAssVS_items)),
    CTS_PhysAssPS = rowMeans(select(., PhysAssPS_items)),
    CTS_Victim = rowMeans(select(., Victim_items)),
    CTS_Perp = rowMeans(select(., Perp_items))
  )
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  
  return(df)
}