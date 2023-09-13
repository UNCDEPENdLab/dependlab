#' Score the Conflict Tactics Scale (CTS)
#'
#' @param df a data.frame containing the 40 CTS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "CTS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per subscale. Default: 0.2
#'            Below this, the mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the lowest value for the items anchors, used to check response validity. Default: 1
#' @param max_value the highest value for the items anchors, used to check response validity. Default: 7
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for subscales and return a column attribute. Default: TRUE
#'
#' @details
#'
#' Adds fifteen columns, twelve subscales, two domain scales, and one total, to \code{df}.
#'
#' Note: the default CTS scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:40
#' will yield the 40 items from the test.
#'
#' @export
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate
#'
score_cts <- function(df, item_prefix="CTS_", max_impute=0.2, drop_items=FALSE,
                      min_value=1, max_value=7, bad_items=NULL, add_alphas=TRUE) {


  #validate data.frame and items
  orig_items <- paste0(item_prefix, 1:40) #expected item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }

  #subscales
  #DEFINTIONS: V=victim, P=perpetrator, M=minor, S=severe
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

  #total items
  total_items <- paste0(item_prefix, 1:40)

  #NB. There is no reverse scoring for the CTS

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
    bad_items <- paste0(item_prefix, bad_items)
    PsychAggV_items <- setdiff(PsychAggV_items,bad_items)
    PsychAggP_items <- setdiff(PsychAggP_items,bad_items)
    PhysAssV_items <- setdiff(PhysAssV_items,bad_items)
    PhysAssP_items <- setdiff(PhysAssP_items,bad_items)
    PsychAggVM_items <- setdiff(PsychAggVM_items,bad_items)
    PsychAggPM_items <- setdiff(PsychAggPM_items,bad_items)
    PsychAggVS_items <- setdiff(PsychAggVS_items,bad_items)
    PsychAggPS_items <- setdiff(PsychAggPS_items,bad_items)
    PhysAssVM_items <- setdiff(PhysAssVM_items,bad_items)
    PhysAssPM_items <- setdiff(PhysAssPM_items,bad_items)
    PhysAssVS_items <- setdiff(PhysAssVS_items,bad_items)
    PhysAssPS_items <- setdiff(PhysAssPS_items,bad_items)
    Victim_items <- setdiff(Victim_items,bad_items)
    Perp_items <- setdiff(Perp_items,bad_items)
    total_items <- setdiff(total_items,bad_items)
  }

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

  #compute row means
  df <- df %>% mutate(
    CTS_PsychAggV = rowMeans(across(all_of(PsychAggV_items))),
    CTS_PsychAggP = rowMeans(across(all_of(PsychAggP_items))),
    CTS_PhysAssV = rowMeans(across(all_of(PhysAssV_items))),
    CTS_PhysAssP = rowMeans(across(all_of(PhysAssP_items))),
    CTS_PsychAggVM = rowMeans(across(all_of(PsychAggVM_items))),
    CTS_PsychAggPM = rowMeans(across(all_of(PsychAggPM_items))),
    CTS_PsychAggVS = rowMeans(across(all_of(PsychAggVS_items))),
    CTS_PsychAggPS = rowMeans(across(all_of(PsychAggPS_items))),
    CTS_PhysAssVM = rowMeans(across(all_of(PhysAssVM_items))),
    CTS_PhysAssPM = rowMeans(across(all_of(PhysAssPM_items))),
    CTS_PhysAssVS = rowMeans(across(all_of(PhysAssVS_items))),
    CTS_PhysAssPS = rowMeans(across(all_of(PhysAssPS_items))),
    CTS_Victim = rowMeans(across(all_of(Victim_items))),
    CTS_Perp = rowMeans(across(all_of(Perp_items))),
    CTS_total = rowMeans(across(all_of(total_items)))
  )

  #compute alphas
  if (add_alphas) {
    attr(df$CTS_PsychAggV, "alpha") <- psych::alpha(df[,PsychAggV_items],max=100,warnings = F)$total
    attr(df$CTS_PsychAggP, "alpha") <- psych::alpha(df[,PsychAggP_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssV, "alpha") <- psych::alpha(df[,PhysAssV_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssP, "alpha") <- psych::alpha(df[,PhysAssP_items],max=100,warnings = F)$total
    attr(df$CTS_PsychAggVM, "alpha") <- psych::alpha(df[,PsychAggVM_items],max=100,warnings = F)$total
    attr(df$CTS_PsychAggPM, "alpha") <- psych::alpha(df[,PsychAggPM_items],max=100,warnings = F)$total
    attr(df$CTS_PsychAggVS, "alpha") <- psych::alpha(df[,PsychAggVS_items],max=100,warnings = F)$total
    attr(df$CTS_PsychAggPS, "alpha") <- psych::alpha(df[,PsychAggPS_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssVM, "alpha") <- psych::alpha(df[,PhysAssVM_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssPM, "alpha") <- psych::alpha(df[,PhysAssPM_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssVS, "alpha") <- psych::alpha(df[,PhysAssVS_items],max=100,warnings = F)$total
    attr(df$CTS_PhysAssPS, "alpha") <- psych::alpha(df[,PhysAssPS_items],max=100,warnings = F)$total
    attr(df$CTS_Victim, "alpha") <- psych::alpha(df[,Victim_items],max=100,warnings = F)$total
    attr(df$CTS_Perp, "alpha") <- psych::alpha(df[,Perp_items],max=100,warnings = F)$total
    attr(df$CTS_total, "alpha") <- psych::alpha(df[,total_items],max=100,warnings = F)$total
  }


  #drop item-level data
  if (drop_items) { df <- df %>% select(-orig_items) }

  return(df)
}
