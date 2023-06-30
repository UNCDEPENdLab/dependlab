#' Score the Borderline Personality Questionnaire (BPQ)
#'
#' @param df a data.frame containing the 80 BPQ items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "BPQ"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param input_codes the original T/F coding scheme of the input dataframe. These are converted to the specified min and max value.
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 0
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 1
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for subscales and return a column attribute. Default: TRUE
#'
#' @details
#'
#' Adds ten columns, \code{BPQ_impulsivity}, \code{BPQ_instability}, \code{BPQ_abandonment}, \code{BPQ_relationships}, \code{BPQ_selfimage}, \code{BPQ_suicide}, \code{BPQ_emptiness}, \code{BPQ_anger}, \code{BPQ_psychotic}, and \code{BPQ_total}, to \code{df} containing
#' the different subscales, respectively.
#'
#' Note: the default BPQ scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:80
#' will yield the 80 items from the test.
#'
#'
#' @export
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate setdiff
#'
score_bpq <- function(df, item_prefix="BPQ_", max_impute=0.2,
                      drop_items=FALSE, keep_reverse_codes=FALSE, input_codes=c(0,1),
                      min_value=0, max_value=1, bad_items = NULL,
                      add_alphas=TRUE) {

  #validate data.frame and items
  orig_items <- paste0(item_prefix, 1:80) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #convert values
  df[,orig_items] <-  data.frame(ifelse(df[,orig_items] == max(input_codes),max_value,min_value))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }

  #set-up reverse coding
  reverse_keys <- c(4, 8, 10, 28, 32, 43, 45, 48, 52, 53, 54, 60, 67) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items

  #define variables and score items. add 'r' suffix to reverse items as needed
  impuls_items <- sapply(c(1, 10, 26, 34, 42, 57, 64, 68, 71), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #impulsivity
  instab_items <- sapply(c(2, 11, 19, 27, 35, 43, 49, 58, 65, 72), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #affective instability
  abandon_items <- sapply(c(3, 12, 20, 28, 44, 50, 59, 66, 73, 78), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #abandonment
  relations_items <- sapply(c(4, 13, 21, 29, 36, 45, 51, 60), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #relationships
  self_items <- sapply(c(5, 14, 37, 46, 52, 61, 67, 70, 74), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #self-image
  suicide_items <- sapply(c(6, 15, 22, 30, 38, 53, 75), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #suicide/self-mutilation
  empti_items <- sapply(c(7, 16, 23, 31, 39, 54, 62, 69, 76, 79), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #emptiness
  anger_items <- sapply(c(8, 17, 24, 32, 40, 47, 55, 63, 77, 80), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #intense anger
  psycho_items <- sapply(c(9, 18, 25, 33, 41, 48, 56), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #quasi-psychotic states
  total_items <- sapply(1:80, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #total

  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-5 scoring by default

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
    bad_items <- sapply(bad_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) })
    impuls_items <- setdiff(impuls_items,bad_items)
    instab_items <- setdiff(instab_items,bad_items)
    abandon_items <- setdiff(abandon_items,bad_items)
    relations_items <- setdiff(relations_items,bad_items)
    self_items <- setdiff(self_items,bad_items)
    suicide_items <- setdiff(suicide_items,bad_items)
    empti_items <- setdiff(empti_items,bad_items)
    anger_items <- setdiff(anger_items,bad_items)
    psycho_items <- setdiff(psycho_items,bad_items)
    total_items <- setdiff(total_items,bad_items)
  }

  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, impuls_items, thresh=max_impute)
    df <- mean_impute_items(df, instab_items, thresh=max_impute)
    df <- mean_impute_items(df, abandon_items, thresh=max_impute)
    df <- mean_impute_items(df, relations_items, thresh=max_impute)
    df <- mean_impute_items(df, self_items, thresh=max_impute)
    df <- mean_impute_items(df, suicide_items, thresh=max_impute)
    df <- mean_impute_items(df, empti_items, thresh=max_impute)
    df <- mean_impute_items(df, anger_items, thresh=max_impute)
    df <- mean_impute_items(df, psycho_items, thresh=max_impute)
  }

  #compute row sums
  df <- df %>% mutate(
    BPQ_impuls = rowSums(across(all_of(impuls_items))),
    BPQ_instab = rowSums(across(all_of(instab_items))),
    BPQ_abandon = rowSums(across(all_of(abandon_items))),
    BPQ_relations = rowSums(across(all_of(relations_items))),
    BPQ_self = rowSums(across(all_of(self_items))),
    BPQ_suicide = rowSums(across(all_of(suicide_items))),
    BPQ_empti = rowSums(across(all_of(empti_items))),
    BPQ_anger = rowSums(across(all_of(anger_items))),
    BPQ_psycho = rowSums(across(all_of(psycho_items))),
    BPQ_total = rowSums(across(all_of(total_items)))
    )

  #compute alphas
  if (add_alphas) {
    attr(df$BPQ_impuls,"alpha") <- psych::alpha(df[,impuls_items])
    attr(df$BPQ_instab,"alpha") <- psych::alpha(df[,instab_items])
    attr(df$BPQ_abandon,"alpha") <- psych::alpha(df[,abandon_items])
    attr(df$BPQ_relations,"alpha") <- psych::alpha(df[,relations_items])
    attr(df$BPQ_self,"alpha") <- psych::alpha(df[,self_items])
    attr(df$BPQ_suicide,"alpha") <- psych::alpha(df[,suicide_items])
    attr(df$BPQ_empti,"alpha") <- psych::alpha(df[,empti_items])
    attr(df$BPQ_anger,"alpha") <- psych::alpha(df[,anger_items])
    attr(df$BPQ_psycho,"alpha") <- psych::alpha(df[,psycho_items])
    attr(df$BPQ_total,"alpha") <- psych::alpha(df[,total_items])
  }

  #drop reverse codes and item-level data
  if (!keep_reverse_codes) { df <- df %>% dplyr::select(-all_of(reverse_items_recode)) }
  if (drop_items) { df <- df %>% dplyr::select(-all_of(orig_items)) }

  return(df)
}
