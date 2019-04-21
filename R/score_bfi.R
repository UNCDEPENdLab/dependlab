#' Score The Big Five Inventory-2 -- 60 items BFI-2 (Soto & John, 2017)
#' 
#' @param df a data.frame containing the 60 BFI-2 items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "BFI"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_reverse_codes whether to retain the reverse coded items (suffix "r")
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 1
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 5
#' 
#' @details 
#' 
#' Adds twenty columns, fifteen for facet scales and five for domain scales, to \code{df} containing
#' the BFI-2 subscales, respectively.
#'
#' Note: the default BFI-2 scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:60
#' will yield the 60 items from the test.
#' 
#' See: \url{http://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/bfi2-form.pdf}
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_bfi <- function(df, item_prefix="BFI", max_impute=0.2, 
                       drop_items=FALSE, keep_reverse_codes=FALSE, min_value=1, max_value=5) {
  
  orig_items <- paste0(item_prefix, 1:60) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  reverse_keys <- c(3,4,5,8,9,11,12,16,17,22,23,24,25,26,28,29,30,31,36,37,42,44,47,48,49,50,51,55,58) #numeric values of items to reverse key
  reverse_items <- paste0(item_prefix, reverse_keys) #names of items to reverse key
  reverse_items_recode <- sub("$", "r", reverse_items, perl=TRUE) #output name for reversed items
  
  #facet items
  soci_items <- sapply(c(1, 16, 31, 46), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #sociability
  assrt_items <- sapply(c(6, 21, 36, 51), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #assertiveness
  enrgy_items <- sapply(c(11, 26, 41 ,56), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #energy level
  comp_items <- sapply(c(2, 17, 32, 47), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #compassion
  respect_items <- sapply(c(7, 22, 37, 52), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #respectfulness
  trust_items <- sapply(c(12, 27, 42, 57), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #trust
  org_items <- sapply(c(3, 18, 33, 48), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #organization
  prod_items <- sapply(c(8, 23, 38, 53), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #productiveness
  respon_items <- sapply(c(13, 28, 43, 58), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #responsibility
  anx_items <- sapply(c(4, 19, 34, 49), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #anxiety
  dep_items <- sapply(c(9, 24, 39, 54), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #depression
  emo_vol_items <- sapply(c(14, 29, 44, 59), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #emotional volatility
  int_cur_items <- sapply(c(10, 25, 40, 55), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #intellectual curiosity
  aes_sens_items <- sapply(c(5, 20, 35, 50), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #aesthetic sensitivity
  creat_imag_items <- sapply(c(15, 30, 45, 60), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #creative imagination

  #domain scales
  extra_items <- sapply(c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #extraversion
  agree_items <- sapply(c(2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 57), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #agreeableness
  cons_items <- sapply(c(3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53, 58), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #conscientiousness
  neg_emo_items <- sapply(c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #negative-emotionality
  open_mind_items <- sapply(c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60), function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "r", "")) }) #open-mindedness

  
  #apply reverse scoring
  df[,reverse_items_recode] <- lapply(df[,reverse_items], function(x) { max_value + min_value - x }) #1-5 scoring by default
  
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, soci_items, thresh=max_impute)
    df <- mean_impute_items(df, assrt_items, thresh=max_impute)
    df <- mean_impute_items(df, enrgy_items, thresh=max_impute)
    df <- mean_impute_items(df, comp_items, thresh=max_impute)
    df <- mean_impute_items(df, respect_items, thresh=max_impute)
    df <- mean_impute_items(df, trust_items, thresh=max_impute)
    df <- mean_impute_items(df, org_items, thresh=max_impute)
    df <- mean_impute_items(df, prod_items, thresh=max_impute)
    df <- mean_impute_items(df, respon_items, thresh=max_impute)
    df <- mean_impute_items(df, anx_items, thresh=max_impute)
    df <- mean_impute_items(df, dep_items, thresh=max_impute)
    df <- mean_impute_items(df, emo_vol_items, thresh=max_impute)
    df <- mean_impute_items(df, int_cur_items, thresh=max_impute)
    df <- mean_impute_items(df, aes_sens_items, thresh=max_impute)
    df <- mean_impute_items(df, creat_imag_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    BFI_soci = rowMeans(select(., soci_items)),
    BFI_assrt = rowMeans(select(., assrt_items)),
    BFI_enrgy = rowMeans(select(., enrgy_items)),
    BFI_comp = rowMeans(select(., comp_items)),
    BFI_respect = rowMeans(select(., respect_items)),
    BFI_trust = rowMeans(select(., trust_items)),
    BFI_org = rowMeans(select(., org_items)),
    BFI_prod = rowMeans(select(., prod_items)),
    BFI_respons = rowMeans(select(., respons_items)),
    BFI_anx = rowMeans(select(., anx_items)),
    BFI_dep = rowMeans(select(., dep_items)),
    BFI_emo_vol = rowMeans(select(., emo_vol_items)),
    BFI_int_cur = rowMeans(select(., int_cur_items)),
    BFI_aes_sens = rowMeans(select(., aes_sens_items)),
    BFI_creat_imag = rowMeans(select(., creat_imag_items)),
    BFI_extra = rowMeans(select(., extra_items)),
    BFI_agree = rowMeans(select(., agree_items)),
    BFI_cons = rowMeans(select(., cons_items)),
    BFI_neg_emo = rowMeans(select(., neg_emo_items)),
    BFI_open_mind = rowMeans(select(., open_mind_items))
  )
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  if (!keep_reverse_codes) { df <- df %>% select(-reverse_items_recode) }
  
  return(df)
}

