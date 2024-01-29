#' Score PAI-BOR subscales for affective instability, indentity problems, negative relationships, and self-harm
#' @description This function scores the PAI-BOR subscales for all subject IDs in the tracker tab of the master spreadsheet.
#' 
#' @param df dataframe containing the PAI-BOR items to be scored
#' @param item_prefix a character prefix of the items names in \code{df} to be scored. Default: "PAIBOR_"
#' @param max_impute the proportion of missingness [0..1) or number [1..n] of missing values per scale.
#'        Below this threshold, the person subscale mean will be imputed for missing items.
#' @param min_value the minimum value for the item anchors, used in reverse scoring. Default: 0
#' @param max_value the highest value for the item anchors, used in reverse scoring. Default: 3
#' 
#' @return A data frame with the PAI-BOR subscales scores and total PAI-BOR score
#' 
#' @export 
#' @author Nidhi Desai
#' 
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom qualtRics fetch_survey
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace_all
#'
score_paibor <- function(df, item_prefix="PAIBOR_", max_impute=0.2, min_value=0, max_value=3) {
    
    orig_items <- paste0(item_prefix, 1:24) #expect item names
    stopifnot(is.data.frame(df))
    stopifnot(all(orig_items %in% names(df)))

    #validate item responses
    responses_valid <- apply(df[,orig_items], 1, function(row) { all(row >= min_value & row <= max_value, na.rm=TRUE) })

    if (any(!responses_valid)) {
    warning("Some responses fall outside of the allowable range: ", min_value, " -- ", max_value, "\n  Returning data.frame unchanged")
    print(df[!responses_valid, orig_items])
    return(df)
    }

    # apply reverse scoring
    reverse_keys <- c(7, 12, 14, 19, 20, 24)
    for(i in reverse_keys){
        df[,paste0(item_prefix, toString(i), "_r")] <- 3 - df[,paste0(item_prefix, toString(i))]
    }

    # define items within each subscale
    ai_items <- c(1, 4, 7, 10, 14, 18) # PAI affective instability subscale items
    ip_items <- c(2, 5, 8, 11, 15, 19) # PAI identity problems subscale items    
    negrel_items <- c(3, 6, 9, 12, 16, 20) # PAI negative relationships subscale items
    selfharm_items <- c(13, 17, 21, 22, 23, 24) # PAI self-harm subscale items
    
    ai_items <- sapply(ai_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "_r", "")) })
    ip_items <- sapply(ip_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "_r", "")) })
    negrel_items <- sapply(negrel_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "_r", "")) })
    selfharm_items <- sapply(selfharm_items, function(x) { paste0(item_prefix, x, ifelse(x %in% reverse_keys, "_r", "")) })
    
    # mean impute, if requested (after reverse scoring to get item direction correct)
    if (max_impute > 0) {
        df <- mean_impute_items(df, ai_items, thresh=max_impute)
        df <- mean_impute_items(df, ip_items, thresh=max_impute)
        df <- mean_impute_items(df, negrel_items, thresh=max_impute)
        df <- mean_impute_items(df, selfharm_items, thresh=max_impute)
    }

    # compute row sums
    df <- df %>% mutate(
        PAIBOR_ai = rowSums(across(all_of(ai_items))),
        PAIBOR_identityprob = rowSums(across(all_of(ip_items))),
        PAIBOR_negrel = rowSums(across(all_of(negrel_items))),
        PAIBOR_selfharm = rowSums(across(all_of(selfharm_items))),
        PAIBOR_total = PAIBOR_ai + PAIBOR_identityprob + PAIBOR_negrel + PAIBOR_selfharm
    )

    # TODO compute alphas 

    return(df)
}