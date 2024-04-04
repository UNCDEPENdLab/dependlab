#' Score the Liebowitz Social Anxiety Scale (LSAS)
#' 
#' @param df a data.frame containing the LSAS items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "LSAS_"
#' @param max_impute the proportion of missingness [0..1] or number [1..n] of missing values per subscale.
#'       Below this, the mean will be imputed for missing items.
#' @param min_value the lowest value for the items anchors, used to check response validity. Default: 0
#' @param max_value the highest value for the items anchors, used to check response validity. Default: 3
#' @param drop_items whether to remove the item-level data from the \code{df}. Default: FALSE
#' @param add_alphas whether to compute coefficient alpha for subscales and return a column attribute. Default: TRUE
#' 
#' @return A data frame with LSAS items, four social interaction & performance fear and avoidance subscales scores and two total fear and avoidance scores.
#' 
#' @export
#' @author Nidhi Desai
#' 
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' 
score_lsas <- function(df, item_prefix="LSAS_", max_impute=0.2, min_value=0, max_value=3, drop_items=FALSE, add_alphas=TRUE) {

    orig_items <- paste0(item_prefix, 1:48) #expect item names
    stopifnot(is.data.frame(df))
    stopifnot(all(orig_items %in% names(df)))

    # validate item responses
    responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

    if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
    }

    # define items within each subscale
    s_ax_items <- paste0(item_prefix, c(9, 13, 19, 21, 23, 29, 35, 37, 43, 45, 47)) # social interaction fear
    p_ax_items <- paste0(item_prefix, c(1, 3, 5, 7, 11, 15, 17, 25, 27, 31, 33, 39, 41)) # performance fear
    s_av_items <- paste0(item_prefix, c(10, 14, 20, 22, 24, 30, 36, 38, 44, 46, 48)) # social interaction avoidance
    p_av_items <- paste0(item_prefix, c(2, 4, 6, 8, 12, 16, 18, 26, 28, 32, 34, 40, 42)) # performance avoidance

    # mean impute, if requested (after reverse scoring to get item direction correct)
    if (max_impute > 0) {
        df <- mean_impute_items(df, s_ax_items, thresh=max_impute)
        df <- mean_impute_items(df, p_ax_items, thresh=max_impute)
        df <- mean_impute_items(df, s_av_items, thresh=max_impute)
        df <- mean_impute_items(df, p_av_items, thresh=max_impute)
    }

    # compute row sums
    df <- df %>% mutate(
        LSAS_social_fear = rowSums(across(all_of(s_ax_items))),
        LSAS_performance_fear = rowSums(across(all_of(p_ax_items))),
        LSAS_social_avoidance = rowSums(across(all_of(s_av_items))),
        LSAS_performance_avoidance = rowSums(across(all_of(p_av_items))),
        LSAS_total_fear = LSAS_social_fear + LSAS_performance_fear,
        LSAS_total_avoidance = LSAS_social_avoidance + LSAS_performance_avoidance
    )
    
    # TODO compute alphas 
    if(add_alphas) {
    }

    # drop items
    if (drop_items) { df <- df %>% select(-all_of(orig_items)) }

    return(df)
}