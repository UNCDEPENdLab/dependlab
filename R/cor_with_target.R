#' This function correlates a set of target variables (target) with a set of other variables (withvars).
#' The general goal is to avoid sifting through gargantuan correlation matrices when only some cells are of interest a priori.
#'
#' @param df The data.frame containing data to be correlated
#' @param omit A vector of variables that should not be included in the \code{withvars}.
#' @param withvars A vector of variables to correlate with the target. If omitted, all variables are included
#' @param pmin The highest correlation p-value that should be included in the output (e.g., to omit non-sig correlations)
#' @param partial A character vector of variable names to partial out of the target correlations
#' @param absrmin The minimum absolute r value to print
#' @param digits The number of digits to print for correlations and p values
#' @param prewhiten Whether to remove the temporal autocorrelation structure of each variable prior correlation (using ARIMA)
#' @param orderbyr Whether to sort the correlation output by the magnitude of the correlation
#'
#' @author Michael Hallquist
#' @return A list of correlations where each element is a vector of a target variable with all withvvars
#' @importFrom forecast auto.arima Arima
#' @importFrom Hmisc rcorr
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

  res <- sapply(target, function(tv) {
        cvec <- sapply(withvars, function(wv) {
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
              list(r=round(rc$r[1,2], 3), p=round(rc$P[1,2], 3))
            }
        )

        if (!is.null(pmin)) {
          sigr <- which(unlist(cvec["p",]) <= pmin)
          if (length(sigr) == 0L) { cvec <- c()
          } else { cvec <- cvec[,sigr, drop=FALSE] }
        }

        if (!is.null(absrmin)) {
          goodr <- which(abs(unlist(cvec["r",])) >= absrmin)
          if (length(goodr) == 0L) { cvec <- c()
          } else { cvec <- cvec[,goodr, drop=FALSE] }
        }

        #be sure that we never include the correlation of the variable with itself
        selfmatch <- dimnames(cvec)[[2]] == tv
        cvec <- cvec[,!selfmatch, drop=FALSE]

        #reorder by correlation size if requested
        if (orderbyr == TRUE) {
          cvec <- cvec[,order(unlist(cvec[1,]), decreasing=TRUE)]
        }

        return(cvec)

        #print(cvec)
      }, simplify=FALSE)

  return(res)
}
