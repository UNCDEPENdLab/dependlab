#' Score the 12-item Super-Brief Pathological Narcissism Inventory (SB-PNI)
#'
#' @param df a data.frame containing the 12-item SBPNI items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "SBPNI"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 7
#'
#' @details
#'
#' Adds two columns, \code{SBPNI_Grandiosity} and \code{SBPNI_Vulnerability}, to \code{df} containing
#' the grandiosity and vulnerability scales, respectively.
#'
#' Note: the default SBPNI scoring uses the mean of the items for the scales.
#'
#' Scale content:
#' ----
#' SBPNI01: (22G)  Right now, I am feeling important because others are relying on me.
#' SBPNI02: (8V)   Right now, I feel bad about myself because others are not noticing me.
#' SBPNI03: (26G)  Right now, I am fantasizing about accomplishing things that are probably beyond my means.
#' SBPNI04: (17V)  Right now, I am avoiding people because I am concerned that they will disappoint me.
#' SBPNI05: (31G)  Right now, I am fantasizing about being rewarded for my efforts.
#' SBPNI06: (30V)  Right now, it is hard to feel good about myself because I do not know if other people are admiring me.
#' SBPNI07: (33G)  Right now, I like to have friends who rely on me because it makes me feel important.
#' SBPNI08: (32V)  Right now, I am preoccupied with thoughts and concerns that most people are not interested in me.
#' SBPNI09: (42G)  Right now, I am fantasizing about performing heroic deeds.
#' SBPNI10: (36V)  Right now, it's hard for me to feel good about myself because I do not know if other people like me.
#' SBPNI11: (45G)  Right now, I am fantasizing about being recognized for my accomplishments.
#' SBPNI12: (50V)  Right now, I feel anxious and ashamed because others got a glimpse of my needs.
#'
#' @export
#' @author Michael Hallquist
#'
#' @importFrom dplyr select mutate
#'
score_sbpni <- function(df, item_prefix="SBPNI", max_impute=0.2,
                      drop_items=FALSE, min_value=1, max_value=100) {

  orig_items <- paste0(item_prefix, sprintf("%02d", 1:12)) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #no reverse scoring for SBPNI
  grandiosity_items <- orig_items[c(1, 3, 5, 7, 9, 11)]
  vulnerability_items <- orig_items[c(2, 4, 6, 8, 10, 12)]

  #mean impute, if requested
  if (max_impute > 0) {
    df <- mean_impute_items(df, grandiosity_items, thresh=max_impute)
    df <- mean_impute_items(df, vulnerability_items, thresh=max_impute)
  }

  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(SBPNI_Grandiosity = rowMeans(select(., grandiosity_items)),
                      SBPNI_Vulnerability = rowMeans(select(., vulnerability_items)),
                      SBPNI_PathNarcissism = rowMeans(cbind(SBPNI_Grandiosity, SBPNI_Vulnerability)))

  if (drop_items) { df <- df %>% select(-orig_items) }

  return(df)
}
