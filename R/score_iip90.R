#' Score the Inventory for Interpersonal Problems -- 90 items PD + circumplex version
#'
#' @param df a data.frame containing the 90 IIP items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "IIP"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per subscale.
#'            Below this, the mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param keep_octants whether to retain the IIP octant scores. Default: FALSE
#' @param min_value the lowest value for the items anchors, used to check response validity. Default: 0
#' @param max_value the highest value for the items anchors, used to check response validity. Default: 4
#'
#' @details
#'
#' UPDATE ME!
#' Adds two columns, \code{ECR_anxiety} and \code{ECR_avoidance}, to \code{df} containing
#' the anxiety and avoidance scales, respectively.
#'
#' Note: the default ECR scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:36
#' will yield the 36 items from the test.
#'
#' @export
#' @author Michael Hallquist
#'
#' @importFrom dplyr select mutate
#'
score_iip90 <- function(df, item_prefix="IIP", max_impute=0.2,
                      drop_items=FALSE, keep_reverse_codes=FALSE, min_value=0, max_value=4) {

  warning("This function is not complete yet. Just returning original data.frame for now.")
  return(df)

  orig_items <- paste0(item_prefix, 1:90) #expect item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }
  
  #octant items
  pa_items <- paste0(item_prefix, c(21, 40, 57, 58, 65, 68, 76, 80))
  bc_items <- paste0(item_prefix, c(1, 26, 28, 38, 41, 50, 73, 88))
  de_items <- paste0(item_prefix, c(11, 18, 20, 24, 27, 31, 46, 82))
  fg_items <- paste0(item_prefix, c(3, 7, 17, 22, 43, 45, 71, 85))
  hi_items <- paste0(item_prefix, c(5, 6, 8, 9, 12, 15, 23, 49))
  jk_items <- paste0(item_prefix, c(2, 10, 29, 44, 48, 54, 69, 83))
  lm_items <- paste0(item_prefix, c(25, 37, 47, 59, 64, 67, 70, 87))
  no_items <- paste0(item_prefix, c(4, 30, 39, 52, 56, 61, 62, 78))

  all_oct <- c(pa_items, bc_items, de_items, fg_items, hi_items, jk_items, lm_items, no_items)
  
  #PD scales
  bpd_items <- paste0(item_prefix, c(51, 53, 55, 66, 77, 80, 89, 90)) #Clifton BPD scale
  sensitivity_pd1_items <- paste0(item_prefix, c(1, 35, 36, 42, 51, 55, 60, 78, 79, 81, 86)) #Pilkonis PD1
  ambivalence_pd2_items <- paste0(item_prefix, c(13, 14, 26, 28, 32, 34, 38, 40, 41, 84)) #Pilkonis PD2
  aggression_pd3_items <- paste0(item_prefix, c(50, 53, 58, 63, 77, 80, 88)) #Pilkonis PD3
  approval_c1_items <- paste0(item_prefix, c(2, 9, 16, 48, 59, 66, 72, 74, 75)) #need for social approval
  lacksocial_c2_items <- paste0(item_prefix, c(3, 7, 17, 19, 22, 33, 43, 49, 71, 85)) #lack of sociability

  #okay to impute within scale
  #bpd_items, sensitivity_pd1_items
  
  #too overlapping with octants, could lead to compound imputation dilemmas
  pd_items <- c(ambivalence_pd2_items, aggression_pd3_items , approval_c1_items, lacksocial_c2_items)
    
  pd_uniq <- pd_items[!pd_items %in% all_oct]
  
  #NB. There is no reverse scoring for the IIP-90

  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, pa_items, thresh=max_impute)
    df <- mean_impute_items(df, bc_items, thresh=max_impute)
    df <- mean_impute_items(df, de_items, thresh=max_impute)
    df <- mean_impute_items(df, fg_items, thresh=max_impute)
    df <- mean_impute_items(df, hi_items, thresh=max_impute)
    df <- mean_impute_items(df, jk_items, thresh=max_impute)
    df <- mean_impute_items(df, lm_items, thresh=max_impute)
    df <- mean_impute_items(df, no_items, thresh=max_impute)
    
    df <- mean_impute_items(df, bpd_items, thresh=max_impute) #Mostly non-overlapping. Only iip80 overlaps octants
    df <- mean_impute_items(df, sensitivity_pd1_items, thresh=max_impute) #Mostly non-overlapping. On iip1 and iip78 overlap octants
    
    if (any(which_miss <- sapply(df[,pd_uniq], function(col) { any(is.na(col)) }))) {
      message("Missing data in items that are unique to PD scales, but where other subscale items overlap octants.")
      message("We will not impute these. Check columns: ", paste(pd_uniq[which_miss], collapse=", "))
    }
  }

  #https://github.com/jennybc/row-oriented-workflows/blob/master/ex09_row-summaries.md
  df <- df %>% mutate(
    IIP_pa = rowMeans(select(., pa_items)),
    IIP_bc = rowMeans(select(., bc_items)),
    IIP_de = rowMeans(select(., de_items)),
    IIP_fg = rowMeans(select(., fg_items)),
    IIP_hi = rowMeans(select(., hi_items)),
    IIP_jk = rowMeans(select(., jk_items)),
    IIP_lm = rowMeans(select(., lm_items)),
    IIP_no = rowMeans(select(., no_items)),
    IIP_bpd = rowMeans(select(., bpd_items)),
    IIP_sensitivity_pd1 = rowMeans(select(., sensitivity_pd1_items)),
    IIP_ambivalence_pd2 = rowMeans(select(., ambivalence_pd2_items)),
    IIP_aggression_pd3 = rowMeans(select(., aggression_pd3_items)),
    IIP_pd = mean(c(IIP_sensitivity_pd1, IIP_ambivalence_pd2, IIP_aggression_pd3)), #overall pd mean
    IIP_havePD = as.numeric(IIP_pd > 1.1), # Pilkonis 1996 cutoff: IIP_havePD > 1.1
    IIP_approval_c1 = rowMeans(select(., approval_c1_items)), #need for social approval
    IIP_lacksocial_c2 = rowMeans(select(., lacksocial_c2_items)), #lack of sociability
    IIP_c = mean(c(IIP_approval_c1, IIP_lacksocial_c2)),
    #THESE MAY NEED TO USE apply(., 1, function(row)) type syntax: https://community.rstudio.com/t/calculate-mean-over-a-subset-of-multiple-specific-variables-but-without-stating-variables-names/7686/6
    IIP_agency = .25*(IIP_pa - IIP_hi + .707*(IIP_bc + IIP_no - IIP_fg - IIP_jk)), #agency axis
    IIP_communion = .25*(IIP_lm - IIP_de + .707*(IIP_no + IIP_jk - IIP_bc - IIP_fg)), #communion axis
    IIP_elevation = (IIP_pa + IIP_bc + IIP_de + IIP_fg + IIP_hi + IIP_jk + IIP_lm + IIP_no)/8 #overall severity (mean of octants)
  )

  if (drop_items) { df <- df %>% select(-orig_items) }

  return(df)
}
