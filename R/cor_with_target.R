#' This function correlates a set of target variables (target) with a set of other variables (withvars).
#' The general goal is to avoid sifting through gargantuan correlation matrices when only some cells are of interest a priori.
#'
#' @param df The data.frame containing data to be correlated
#' @param omit A vector of variables that should not be included in the \code{withvars}.
#' @param target A vector of variable names that are correlated with \code{withvars}.
#' @param withvars A vector of variable names, each of which is correlates with each \code{target}. If omitted, all variables are included.
#' @param pmin The highest correlation p-value that should be included in the output (e.g., to omit non-sig correlations)
#' @param partial A character vector of variable names to partial out of the target correlations
#' @param absrmin The minimum absolute r value to print
#' @param digits The number of digits to return for correlations and p values
#' @param prewhiten Whether to remove the temporal autocorrelation structure of each variable prior correlation (using ARIMA)
#' @param orderbyr Whether to sort the correlation output by the magnitude of the correlation
#'
#' @author Michael Hallquist
#' @return A list of correlations where each element is a vector of a target variable with all withvvars
#' @importFrom forecast auto.arima Arima
#' @importFrom Hmisc rcorr
#' @importFrom dplyr arrange mutate select filter desc bind_rows
#' @export

cor_with_target <- function(df, omit=NULL, target, withvars=NULL, pmin=NULL, partial=NULL, absrmin=NULL, digits=3, prewhiten=FALSE, orderbyr=FALSE) {

  if (!is.null(omit)) {
    dnames <- which(names(df) %in% omit)
    df <- df[,-1*dnames]
  }

  if (is.null(withvars)) {
    withvars <- names(df)[which(!names(df) %in% target)]
  }

  if (!is.null(partial)) {
    df <- as.data.frame(lapply(df, function(col) {
      residuals(lm(col ~ as.matrix(df[,partial])))
    }))
  }

  res <- lapply(target, function(tv) {
    corr_df <- lapply(withvars, function(wv) {
      #prewhiten?
      if (prewhiten) {
        r <- residuals(lm(df[,tv] ~ df[,wv]))
        a <- auto.arima(r)
        x <- Arima(df[,wv], model=a)$residuals
        y <- Arima(df[,tv], model=a)$residuals
      } else {
        x <- df[,wv]
        y <- df[,tv]
      }

      tryCatch(rc <- Hmisc::rcorr(x, y), error=function(e) { print(e); browser() } )
      data.frame(targetvar=tv, withvar=wv, r=round(rc$r[1,2], digits), p=round(rc$P[1,2], digits), stringsAsFactors = FALSE)
    })

    corr_df <- bind_rows(corr_df) #convert to single data.frame (not list of one-row dfs)

    if (!is.null(pmin)) { #enforce maximum p-value
      corr_df <- corr_df %>% dplyr::filter(p <= pmin)
    }

    if (!is.null(absrmin)) { #enforce minimum |r|
      corr_df <- corr_df %>% dplyr::mutate(abs_r=abs(r)) %>% dplyr::filter(abs_r >= absrmin) %>% dplyr::select(-abs_r)
    }

    #be sure that we never include the correlation of the variable with itself
    corr_df <- corr_df %>% filter(targetvar != withvar)

    #reorder by correlation size if requested
    if (orderbyr == TRUE) {
      corr_df <- corr_df %>% dplyr::arrange(desc(r))
    }

    return(corr_df)
  })

  res <- bind_rows(res)
  return(res)
}
