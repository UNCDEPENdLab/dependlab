#' Score the Personality Inventory for DSM-5 Faceted Brief Form -- 100 items PID-5-BF (Maples et al., 2013)
#' 
#' @param df a data.frame containing the 100 PID-5-BF items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "PID"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 0
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 3
#' 
#' @details 
#' 
#' Adds thirty columns to \code{df} containing the different trait facet and trait domain subscales, respectively.
#'
#' Note: the default PID scoring uses the mean of the items for the scales.
#' 
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:100
#' will yield the 100 items from the test.
#' 
#' 
#' @export
#' @author Michael Hallquist
#' 
#' @importFrom dplyr select mutate
#' 
score_pid5 <- function(df, item_prefix="PID", max_impute=0.2, 
                       drop_items=FALSE, keep_reverse_codes=FALSE, min_value=0, max_value=3) {
  
  orig_items <- paste0(item_prefix, 1:100) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  
  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })
  
  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  
  #personality trait facet items
  anh_items <- paste0(item_prefix, c(9, 11, 43, 65)) #anhedonia
  anx_items <- paste0(item_prefix, c(24, 36, 48, 78)) #anxiousness
  attn_items <- paste0(item_prefix, c(23, 77, 87, 97)) #attention seeking
  callous_items <- paste0(item_prefix, c(7, 62, 72, 82)) #callousness
  deceit_items <- paste0(item_prefix, c(18, 51, 95, 99)) #deceitfulness
  depres_items <- paste0(item_prefix, c(26, 60, 70, 74)) #depressivity
  distr_items <- paste0(item_prefix, c(39, 49, 55, 91)) #distractability
  ecc_items <- paste0(item_prefix, c(10, 22, 61, 94)) #eccentricity
  emo_lab_items <- paste0(item_prefix, c(41, 53, 71, 81)) #emotional lability
  grnd_items <- paste0(item_prefix, c(14, 37, 85, 90)) #grandiosity
  host_items <- paste0(item_prefix, c(12, 31, 66, 75)) #hostility
  impuls_items <- paste0(item_prefix, c(2, 5, 6, 8)) #impulsivity
  int_avd_items <- paste0(item_prefix, c(29, 40, 56, 93)) #intimacy avoidance
  irr_items <- paste0(item_prefix, c(47, 64, 68, 76)) #irresponsibility
  man_items <- paste0(item_prefix, c(35, 44, 69, 100)) #manipulativeness
  perc_dysreg_items <- paste0(item_prefix, c(15, 63, 88, 98)) #perceived dysregulation
  persev_items <- paste0(item_prefix, c(19, 25, 32, 46)) #perseveration
  rest_aff_items <- paste0(item_prefix, c(28, 30, 73, 83)) #restricted affect
  perf_items <- paste0(item_prefix, c(33, 42, 80, 89)) #rigid perfectionism
  rt_items <- paste0(item_prefix, c(13, 16, 21, 67)) #risk-taking
  sep_insec_items <- paste0(item_prefix, c(17, 45, 58, 79)) #separation insecurity
  sub_items <- paste0(item_prefix, c(3, 4, 20, 92)) #submissiveness
  sus_items <- paste0(item_prefix, c(1, 38, 50, 86)) #suspiciousness
  unusual_items <- paste0(item_prefix, c(34, 54, 59, 96)) #unusual beliefs and experiences
  wthdrwl_items <- paste0(item_prefix, c(27, 52, 57, 84)) #withdrawal
  
  
  #personality trait domain items
  neg_aff_items <- paste0(item_prefix, c(17, 24, 36, 41, 45, 48, 53, 58, 71, 78, 79, 81)) #negative affect (emotional lability, anxiousness, separation insecurity)
  detach_items <- paste0(item_prefix, c(9, 11, 27, 29, 40, 43, 52, 56, 57, 65, 84, 93)) #detachment (withdrawal, anhedonia, intimacy avoidance)
  antag_items <- paste0(item_prefix, c(14, 18, 35, 37, 44, 51, 69, 85, 90, 95, 99, 100)) #antagonism (manipulativeness, deceitfulness, grandiosity)
  disinhib_items <- paste0(item_prefix, c(2, 5, 6, 8, 39, 47, 49, 55, 64, 68, 76, 91)) #disinhibition (irresponsibility, impulsivity, distractability)
  psycho_items <- paste0(item_prefix, c(10, 15, 22, 34, 54, 59, 61, 63, 88, 94, 96, 98)) #psychoticism (unusual beliefs & experiences, eccentricity, perceptual dysregulation)
  
  #NB. There is no reverse scoring for the PID-5-BF
  
  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, anh_items, thresh=max_impute)
    df <- mean_impute_items(df, anx_items, thresh=max_impute)
    df <- mean_impute_items(df, attn_items, thresh=max_impute)
    df <- mean_impute_items(df, callous_items, thresh=max_impute)
    df <- mean_impute_items(df, deceit_items, thresh=max_impute)
    df <- mean_impute_items(df, depres_items, thresh=max_impute)
    df <- mean_impute_items(df, distr_items, thresh=max_impute)
    df <- mean_impute_items(df, ecc_items, thresh=max_impute)
    df <- mean_impute_items(df, emo_lab_items, thresh=max_impute)
    df <- mean_impute_items(df, grnd_items, thresh=max_impute)
    df <- mean_impute_items(df, host_items, thresh=max_impute)
    df <- mean_impute_items(df, impuls_items, thresh=max_impute)
    df <- mean_impute_items(df, int_avd_items, thresh=max_impute)
    df <- mean_impute_items(df, irr_items, thresh=max_impute)
    df <- mean_impute_items(df, man_items, thresh=max_impute)
    df <- mean_impute_items(df, perc_dysreg_items, thresh=max_impute)
    df <- mean_impute_items(df, persev_items, thresh=max_impute)
    df <- mean_impute_items(df, rest_aff_items, thresh=max_impute)
    df <- mean_impute_items(df, perf_items, thresh=max_impute)
    df <- mean_impute_items(df, rt_items, thresh=max_impute)
    df <- mean_impute_items(df, sep_insec_items, thresh=max_impute)
    df <- mean_impute_items(df, sub_items, thresh=max_impute)
    df <- mean_impute_items(df, sus_items, thresh=max_impute)
    df <- mean_impute_items(df, unusual_items, thresh=max_impute)
    df <- mean_impute_items(df, wthdrwl_items, thresh=max_impute)
  }
  
  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    PID_anh = rowMeans(select(., anh_items)),
    PID_anx = rowMeans(select(., anx_items)),
    PID_attn = rowMeans(select(., attn_items)),
    PID_callous = rowMeans(select(., callous_items)),
    PID_deceit = rowMeans(select(., deceit_items)),
    PID_depres = rowMeans(select(., depres_items)),
    PID_distr = rowMeans(select(., distr_items)),
    PID_ecc = rowMeans(select(., ecc_items)),
    PID_emo_lab = rowMeans(select(., emo_lab_items)),
    PID_grnd = rowMeans(select(., grnd_items)),
    PID_host = rowMeans(select(., host_items)),
    PID_impuls = rowMeans(select(., impuls_items)),
    PID_int_avd = rowMeans(select(., int_avd_items)),
    PID_irr = rowMeans(select(., irr_items)),
    PID_man = rowMeans(select(., man_items)),
    PID_perc_dysreg = rowMeans(select(., perc_dysreg_items)),
    PID_persev = rowMeans(select(., persev_items)),
    PID_rest_aff = rowMeans(select(., rest_aff_items)),
    PID_perf = rowMeans(select(., perf_items)),
    PID_rt = rowMeans(select(., rt_items)),
    PID_sep_insec = rowMeans(select(., sep_insec_items)),
    PID_sub = rowMeans(select(., sub_items)),
    PID_sus = rowMeans(select(., sus_items)),
    PID_unusual = rowMeans(select(., unusual_items)),
    PID_wthdrwl = rowMeans(select(., wthdrwl_items)),
    PID_neg_aff = rowMeans(select(., neg_aff_items)),
    PID_detach = rowMeans(select(., detach_items)),
    PID_antag = rowMeans(select(., antag_items)),
    PID_disinhib = rowMeans(select(., disinhib_items)),
    PID_psycho = rowMeans(select(., psycho_items))
  )
  
  if (drop_items) { df <- df %>% select(-orig_items) }
  
  return(df)
}