#' Score the Flourishing Scale (Diener, Wirtz, Tov, Kim-Prieto, Choi, Oishi, & Biswas-Diener, 2009)
#'
#' @param df a data.frame containing the 8 FS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "FS"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale. Default: 0.2
#'           Below this threshold, the person subscale mean will be imputed for missing items.
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param min_value the minimum value for the item anchors. Default: 1
#' @param max_value the highest value for the item anchors. Default: 7
#' @param bad_items numeric value or vector of the items that need to be dropped before imputation or calculation of subscales
#' @param add_alphas whether to compute coefficient alpha for the total scale and return a column attribute. Default: TRUE
#'
#' @details
#'
#' Adds one column, \code{FS_total}, to \code{df} containing
#' the FS scale items.
#'
#' Note: the default FS scoring uses the mean of the items for the scales.
#'
#' Note: the code assumes that pasting together the \code{item_prefix} and the numbers 1:8
#' will yield the 8 items from the test.
#'
#' See: \url{http://labs.psychology.illinois.edu/~ediener/FS.html}
#'
#' @export
#' @author Michael Hallquist, Zach Vig
#'
#' @importFrom dplyr select mutate setdiff
#'
score_fs <- function(df, item_prefix="FS_", max_impute=0.2, drop_items=FALSE,
                     min_value=1, max_value=7, bad_items=NULL, add_alphas=TRUE) {

  #validate data.frame and items
  orig_items <- paste0(item_prefix, 1:8) #expected item names
  stopifnot(is.data.frame(df))
  stopifnot(all(orig_items %in% names(df)))

  #validate item responses
  responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

  if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
  }

  #NB. There is no reverse scoring for the FS

  #drop bad item(s), before imputation and calculation of scores
  if(!is.null(bad_items) && is.numeric(bad_items)) {
    bad_items <- paste0(item_prefix, bad_items)
    total_items <- setdiff(orig_items,bad_items)
  } else {
    total_items <- orig_items
  }

  #mean impute, if requested (after reverse scoring to get item direction correct)
  if (max_impute > 0) {
    df <- mean_impute_items(df, total_items, thresh=max_impute)
  }

  #compute row sums
  df <- df %>% mutate(
    FS_total = rowSums(across(all_of(total_items)))
  )

  #compute alpha
  if (add_alphas) {
    attr(df[["FS_total"]], "alpha") <- psych::alpha(df[,total_items],max=100,warnings = F)
  }

  #drop item-level data
  if (drop_items) { df <- df %>% select(-all_of(orig_items)) }

  return(df)
}
