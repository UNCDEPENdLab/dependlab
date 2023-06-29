#' Score the Short Version of the Revised Drug Use Screening Inventory (DUSI-R) (Tarter, 1990)
#'
#' @param df a data.frame containing the 15 DUSI-R SF items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "DUSI_"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale. Default: 0.2
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param input_codes the original T/F coding scheme of the input data.frame. These are converted to the specified min and max value.
#' @param min_value the minimum value for the item anchors, used in score validation. Default: 0
#' @param max_value the highest value for the item anchors, used in score validation. Default: 1
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for the total scale and return a column attribute. Default: TRUE
#'
#' @details
#'
#' Adds two columns, \code{DUSI_total} and \code{DUSI_density}, to \code{df} containing the sum of scores and
#' mean of the items for the scales (i.e., the "absolute problem density"), respectively.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:15
#' will yield the 15 items from the test.
#'
#'
#' @export
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate setdiff
#'
score_dusi <- function(df, item_prefix="DUSI_", max_impute=0.2, drop_items=FALSE,
                       input_codes=c(0,1), min_value=0, max_value=1, bad_items=NULL,
                       add_alphas=TRUE) {

  #validate data.frame and items
  orig_items <- paste0(item_prefix, 1:15) #expected item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))
  stopifnot(length(input_codes)==2)

  #convert values, if applicable
  df[,orig_items] <-  data.frame(ifelse(df[,orig_items] == max(input_codes),max_value,min_value))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }

  #NB. There is no reverse scoring for the DUSI

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
      bad_items <- paste0(item_prefix, bad_items)
      total_items <- setdiff(orig_items,bad_items)
  } else {
      total_items <- orig_items
    }

  #mean impute, if requested
  if (max_impute > 0) {
    df <- mean_impute_items(df, total_items, thresh=max_impute) }

  #compute row sums
  df <- df %>% mutate(
    DUSI_total = rowSums(across(all_of(total_items))))

  #compute row means
  df <- df %>% mutate(
    DUSI_density = rowMeans(across(all_of(total_items))))

  #compute alpha
  if (add_alphas) {
    attr(df[["DUSI_total"]], "alpha") <- psych::alpha(df[,total_items])
  }

  #drop item-level data
  if (drop_items) { df <- df %>% select(-all_of(orig_items)) }

  return(df)
}
