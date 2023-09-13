#' Score the Personality Inventory for DSM-5 Faceted Brief Form -- 100 items PID-5-BF (Maples et al., 2013)
#'
#' @param df a data.frame containing the 100 PID-5-BF items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "PID"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 0
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 3
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for the total scale and return a column attribute. Default: TRUE
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
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate
#'
score_pid5 <- function(df, item_prefix="PID_", max_impute=0.2, drop_items=FALSE,
                       min_value=0, max_value=3, bad_items=NULL, add_alphas=TRUE) {

  #validate data.frame and items
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

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
    bad_items <- paste0(item_prefix, bad_items)
    anh_items <- setdiff(anh_items, bad_items)
    anx_items <- setdiff(anx_items, bad_items)
    attn_items <- setdiff(attn_items, bad_items)
    callous_items <- setdiff(callous_items, bad_items)
    deceit_items <- setdiff(deceit_items, bad_items)
    depres_items <- setdiff(depres_items, bad_items)
    distr_items <- setdiff(distr_items, bad_items)
    ecc_items <- setdiff(ecc_items, bad_items)
    emo_lab_items <- setdiff(emo_lab_items, bad_items)
    grnd_items <- setdiff(grnd_items, bad_items)
    host_items <- setdiff(host_items, bad_items)
    impuls_items <- setdiff(impuls_items, bad_items)
    int_avd_items <- setdiff(int_avd_items, bad_items)
    irr_items <- setdiff(irr_items, bad_items)
    man_items <- setdiff(man_items, bad_items)
    perc_dysreg_items <- setdiff(perc_dysreg_items, bad_items)
    persev_items <- setdiff(persev_items, bad_items)
    rest_aff_items <- setdiff(rest_aff_items, bad_items)
    perf_items <- setdiff(perf_items, bad_items)
    rt_items <- setdiff(rt_items, bad_items)
    sep_insec_items <- setdiff(sep_insec_items, bad_items)
    sub_items <- setdiff(sub_items, bad_items)
    sus_items <- setdiff(sus_items, bad_items)
    unusual_items <- setdiff(unusual_items, bad_items)
    wthdrwl_items <- setdiff(wthdrwl_items, bad_items)
    neg_aff_items <- setdiff(neg_aff_items, bad_items)
    detach_items <- setdiff(detach_items, bad_items)
    antag_items <- setdiff(antag_items, bad_items)
    disinhib_items <- setdiff(disinhib_items, bad_items)
    psycho_items <- setdiff(psycho_items, bad_items)
  }

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

  #compute row means
  df <- df %>% mutate(
    PID_anh = rowMeans(across(all_of(anh_items))),
    PID_anx = rowMeans(across(all_of(anx_items))),
    PID_attn = rowMeans(across(all_of(attn_items))),
    PID_callous = rowMeans(across(all_of(callous_items))),
    PID_deceit = rowMeans(across(all_of(deceit_items))),
    PID_depres = rowMeans(across(all_of(depres_items))),
    PID_distr = rowMeans(across(all_of(distr_items))),
    PID_ecc = rowMeans(across(all_of(ecc_items))),
    PID_emo_lab = rowMeans(across(all_of(emo_lab_items))),
    PID_grnd = rowMeans(across(all_of(grnd_items))),
    PID_host = rowMeans(across(all_of(host_items))),
    PID_impuls = rowMeans(across(all_of(impuls_items))),
    PID_int_avd = rowMeans(across(all_of(int_avd_items))),
    PID_irr = rowMeans(across(all_of(irr_items))),
    PID_man = rowMeans(across(all_of(man_items))),
    PID_perc_dysreg = rowMeans(across(all_of(perc_dysreg_items))),
    PID_persev = rowMeans(across(all_of(persev_items))),
    PID_rest_aff = rowMeans(across(all_of(rest_aff_items))),
    PID_perf = rowMeans(across(all_of(perf_items))),
    PID_rt = rowMeans(across(all_of(rt_items))),
    PID_sep_insec = rowMeans(across(all_of(sep_insec_items))),
    PID_sub = rowMeans(across(all_of(sub_items))),
    PID_sus = rowMeans(across(all_of(sus_items))),
    PID_unusual = rowMeans(across(all_of(unusual_items))),
    PID_wthdrwl = rowMeans(across(all_of(wthdrwl_items))),
    PID_neg_aff = rowMeans(across(all_of(neg_aff_items))),
    PID_detach = rowMeans(across(all_of(detach_items))),
    PID_antag = rowMeans(across(all_of(antag_items))),
    PID_disinhib = rowMeans(across(all_of(disinhib_items))),
    PID_psycho = rowMeans(across(all_of(psycho_items)))
  )

  #compute alphas
  if (add_alphas) {
    attr(df$PID_anh,"alpha") <- psych::alpha(df[,anh_items],max=100,warnings = F)$total
    attr(df$PID_anx,"alpha") <- psych::alpha(df[,anx_items],max=100,warnings = F)$total
    attr(df$PID_attn,"alpha") <- psych::alpha(df[,attn_items],max=100,warnings = F)$total
    attr(df$PID_callous,"alpha") <- psych::alpha(df[,callous_items],max=100,warnings = F)$total
    attr(df$PID_deceit,"alpha") <- psych::alpha(df[,deceit_items],max=100,warnings = F)$total
    attr(df$PID_depres,"alpha") <- psych::alpha(df[,depres_items],max=100,warnings = F)$total
    attr(df$PID_distr,"alpha") <- psych::alpha(df[,distr_items],max=100,warnings = F)$total
    attr(df$PID_ecc,"alpha") <- psych::alpha(df[,ecc_items],max=100,warnings = F)$total
    attr(df$PID_emo_lab,"alpha") <- psych::alpha(df[,emo_lab_items],max=100,warnings = F)$total
    attr(df$PID_grnd,"alpha") <- psych::alpha(df[,grnd_items],max=100,warnings = F)$total
    attr(df$PID_host,"alpha") <- psych::alpha(df[,host_items],max=100,warnings = F)$total
    attr(df$PID_impuls,"alpha") <- psych::alpha(df[,impuls_items],max=100,warnings = F)$total
    attr(df$PID_int_avd,"alpha") <- psych::alpha(df[,int_avd_items],max=100,warnings = F)$total
    attr(df$PID_irr,"alpha") <- psych::alpha(df[,irr_items],max=100,warnings = F)$total
    attr(df$PID_man,"alpha") <- psych::alpha(df[,man_items],max=100,warnings = F)$total
    attr(df$PID_perc_dysreg,"alpha") <- psych::alpha(df[,perc_dysreg_items],max=100,warnings = F)$total
    attr(df$PID_persev,"alpha") <- psych::alpha(df[,persev_items],max=100,warnings = F)$total
    attr(df$PID_rest_aff,"alpha") <- psych::alpha(df[,rest_aff_items],max=100,warnings = F)$total
    attr(df$PID_perf,"alpha") <- psych::alpha(df[,perf_items],max=100,warnings = F)$total
    attr(df$PID_rt,"alpha") <- psych::alpha(df[,rt_items],max=100,warnings = F)$total
    attr(df$PID_sep_insec,"alpha") <- psych::alpha(df[,sep_insec_items],max=100,warnings = F)$total
    attr(df$PID_sub,"alpha") <- psych::alpha(df[,sub_items],max=100,warnings = F)$total
    attr(df$PID_sus,"alpha") <- psych::alpha(df[,sus_items],max=100,warnings = F)$total
    attr(df$PID_unusual,"alpha") <- psych::alpha(df[,unusual_items],max=100,warnings = F)$total
    attr(df$PID_wthdrwl,"alpha") <- psych::alpha(df[,wthdrwl_items],max=100,warnings = F)$total
    attr(df$PID_neg_aff,"alpha") <- psych::alpha(df[,neg_aff_items],max=100,warnings = F)$total
    attr(df$PID_detach,"alpha") <- psych::alpha(df[,detach_items],max=100,warnings = F)$total
    attr(df$PID_antag,"alpha") <- psych::alpha(df[,antag_items],max=100,warnings = F)$total
    attr(df$PID_disinhib,"alpha") <- psych::alpha(df[,disinhib_items],max=100,warnings = F)$total
    attr(df$PID_psycho,"alpha") <- psych::alpha(df[,psycho_items],max=100,warnings = F)$total
  }

  #drop item-level data
  if (drop_items) { df <- df %>% select(-orig_items) }

  return(df)
}
