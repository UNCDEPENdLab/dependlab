#' Score the UPPS-P Impulsive Behavior scale -- revised 59 item version (Lynam, Smith, Whiteside, & Cyders, 2007)
#'
#' @param df a data.frame containing the 59 UPPS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "UPPS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 4
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for subscales and return a column attribute. Default: TRUE
#'
#' @details
#'
#' Adds five columns, \code{UPPS_negative_urgency}, \code{UPPS_lack_premeditation}, \code{UPPS_lack_perseverance}, \code{UPPS_sensation_seeking}, \code{UPPS_positive_urgency}, to \code{df} containing
#' the UPPS-P subscales, respectively.
#'
#' Note: the default UPPS scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:59
#' will yield the 59 items from the test.
#'
#' See: \url{http://web.archive.org/web/20190710205448/http://www.impulsivity.org/measurement/UPPS-P_59_item.pdf}
#'
#' @export
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate
#'
score_upps <- function(df, item_prefix="UPPS_", max_impute=0.2, drop_items=FALSE,
                       keep_reverse_codes=FALSE, min_value=1, max_value=4, bad_items=NULL,
                       add_alphas=TRUE) {

  #validate data.frame and items
  orig_items <- paste0(item_prefix, 1:59) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }

  #set-up reverse coding
  reverse_keys <- c(2, 3, 5, 7, 8, 9, 10, 12, 13, 15, 17, 18, 20, 22, 23, 25, 26, 29, 30, 31, 34, 35, 36, 39, 40, 41, 44, 45, 46, 47, 50, 51, 52, 53, 55, 56, 57, 58, 59) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items

  #define variables and score items. add 'r' suffix to reverse items as needed
  negurg_items <- sapply(c(2, 7, 12, 17, 22, 29, 34, 39, 44, 50, 53, 58), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #negative urgency
  lackprem_items <- sapply(c(1, 6, 11, 16, 21, 28, 33, 38, 43, 48, 55), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #lack of premeditation
  lackpers_items <- sapply(c(4, 9, 14, 19, 24, 27, 32, 37, 42, 47), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #lack of perseverance
  senseek_items <- sapply(c(3, 8, 13, 18, 23, 26, 31, 36, 41, 46, 51, 56), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #sensation seeking
  posurg_items <- sapply(c(5, 10, 15, 20, 25, 30, 35, 40, 45, 49, 52, 54, 57, 59), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #positive urgency

  #total_items
  total_items <- sapply(c(1:59), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
    bad_items <- sapply(bad_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })
    negurg_items <- setdiff(negurg_items,bad_items)
    lackprem_items <- setdiff(lackprem_items,bad_items)
    lackpers_items <- setdiff(lackpers_items,bad_items)
    senseek_items <- setdiff(senseek_items,bad_items)
    posurg_items <- setdiff(posurg_items,bad_items)
    total_items <- setdiff(total_items,bad_items)
  }

  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-4 scoring by default

  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, negurg_items, thresh=max_impute)
    df <- mean_impute_items(df, lackprem_items, thresh=max_impute)
    df <- mean_impute_items(df, lackpers_items, thresh=max_impute)
    df <- mean_impute_items(df, senseek_items, thresh=max_impute)
    df <- mean_impute_items(df, posurg_items, thresh=max_impute)
  }

  #compute row means
  df <- df %>% mutate(
    UPPS_negurg = rowMeans(across(all_of(negurg_items))),
    UPPS_lackprem = rowMeans(across(all_of(lackprem_items))),
    UPPS_lackpers = rowMeans(across(all_of(lackpers_items))),
    UPPS_senseek = rowMeans(across(all_of(senseek_items))),
    UPPS_posurg = rowMeans(across(all_of(posurg_items))),
    UPPS_total = rowMeans(across(all_of(total_items)))
  )

  #compute alphas
  if (add_alphas) {
    attr(df$UPPS_negurg,"alpha") <- psych::alpha(df[,negurg_items])
    attr(df$UPPS_lackprem,"alpha") <- psych::alpha(df[,lackprem_items])
    attr(df$UPPS_lackpers,"alpha") <- psych::alpha(df[,lackpers_items])
    attr(df$UPPS_senseek,"alpha") <- psych::alpha(df[,senseek_items])
    attr(df$UPPS_posurg,"alpha") <- psych::alpha(df[,posurg_items])
    attr(df$UPPS_total,"alpha") <- psych::alpha(df[,total_items])
  }

  #drop reverse codes and item-level data
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  if (drop_items) { df <- df %>% select(-orig_items) }

  return(df)
}
