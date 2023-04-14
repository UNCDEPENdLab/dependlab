#' Scores the Dyadic Adjustment Scale (Spanier 1976)
#' This is for the 32-item version
#' 
#' @param df a data.frame containing the 32 DAS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "ECR"
#' @param max_impute the proportion of missingness [0..1) or number [1..] of missing values per scale. Below this, the mean will be imputed for missing items
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param max_value the highest value for the items anchors, used in reverse scoring. Default: 7
#' 
#' @details 
#' 
#' Adds five columns, \code{DASSat},  \code{DASCon}, \code{DASCoh}, \code{DASAffExp}, and \code{DASTotal} to \code{df} containing
#' the different subscaled.
#'
#' Note: the default DAS scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:32
#' will yield the 32 items from the test.
#' 
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 

score_das <- function(df, item_prefix = "DAS", max_impute = .2, drop_items = FALSE, max_value = 7 ) {
                      
                      
                      
                      
reverseItems <- paste(item_prefix, c(1:15, 18, 19, 32), sep="") 
df[,reverseItems] <- lapply(df[,reverseItems], function(x) { 6 - x }) #reverse score and subtract 1 (becomes 0-5)

reverseItems <- paste(item_prefix, c(23, 24), sep="") 
df[,reverseItems] <- lapply(df[,reverseItems], function(x) { 5 - x }) #reverse score and subtract 1 (becomes 0-4)

subtractItems <- paste(item_prefix, c(16, 17, 20:22, 25:28, 31), sep="")
#df$DAS20[das$DAS20 == 7] <- NA #a '7' in the original coding indicates they are not married/live together 
item20 <- paste0(item_prefix, "20")
df[[item20]] <- ifelse(df[[item20]] == 7, NA, df[[item20]])
df[,subtractItems] <- lapply(df[,subtractItems], function(x) { x - 1 })

flipItems <- paste(item_prefix, c(29, 30), sep="")
df[,flipItems] <- lapply(df[,flipItems], function(x) { ifelse(x==1, 0, 1) })

#if DASrelationship is 0, then the person is not in a relationship with their partner (or at least the one at enrollment)
#invalidate any data
df <- df %>% mutate_at(vars(one_of(paste0(item_prefix, 1:32))), funs(ifelse(DASrelationship==0, NA, .)))
con_items <- sapply(c(1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15), function(x) { paste0(item_prefix, x) })
sat_items <- sapply(c(16, 17, 18, 19, 20, 21, 22, 23, 31, 32), function(x) { paste0(item_prefix, x) })
coh_items <- sapply(c(24, 25, 26, 27, 28), function(x) { paste0(item_prefix, x) })
affexp_items <- sapply(c(4, 6, 29, 30), function(x) { paste0(item_prefix, x) })
tot_items <- sapply(seq(1,32, by = 1), function(x) { paste0(item_prefix, x) })

if (max_impute > 0) {
  df <- mean_impute_items(df, con_items, thresh=max_impute)
  df <- mean_impute_items(df, sat_items, thresh=max_impute)
  df <- mean_impute_items(df, coh_items, thresh=max_impute)
  df <- mean_impute_items(df, affexp_items, thresh=max_impute)
  df <- mean_impute_items(df, tot_items, thresh=max_impute)
}


#if(item_prefix == "DAS") {df <- df %>% mutate(
#  DASCon=DAS1 + DAS2 + DAS3 + DAS5 + DAS7 + DAS8 + DAS9 + DAS10 + DAS11 + DAS12 + DAS13 + DAS14 + DAS15, #consensus
#  DASSat=DAS16 + DAS17 + DAS18 + DAS19 + DAS20 + DAS21 + DAS22 + DAS23 + DAS31 + DAS32, #satisfaction
#  DASCoh=DAS24 + DAS25 + DAS26 + DAS27 + DAS28, #cohesion
#  DASAffExp=DAS4 + DAS6 + DAS29 + DAS30, #affectional expression
#  DASTotal=DAS1 + DAS2 + DAS3 + DAS4 + DAS5 + DAS6 + DAS7 + DAS8 + DAS9 + DAS10 + DAS11 + DAS12 + 
#    DAS13 + DAS14 + DAS15 + DAS16 + DAS17 + DAS18 + DAS19 + DAS20 + DAS21 + DAS22 + DAS23 + DAS24 +
#    DAS25 + DAS26 + DAS27 + DAS28 + DAS29 + DAS30 + DAS31 + DAS32
#)} else {
  
  
  df <- df %>% mutate(DASCon = rowMeans(select(., con_items)), DASSat = rowMeans(select(., sat_items)),
                      DASCoh = rowMeans(select(., coh_items)), DASAffExp = rowMeans(select(., affexp_items)),
                      DASTotal = rowMeans(select(., tot_items)))
  
  
  
  
#}

return(df)

}