#potentially add ability to get empirical bayes trajectories per person using coef?

#' This function predicts outcome variable from a (g)lmer object across values of the predictors
#' It plays well with ggplot2 and is intended to give a visual sense of the model-predicted estimates.
#' 
#' By default, lmer_predict will compute estimates of the outcome variable for every combination of predictors
#' that are in the model. For continuous predictors, it finds the range of observed values and computes at
#' 20 equally spaced points along the range. This number is controlled by \code{n.cont}. For categorical predictors,
#' it computes estimates at each level.
#'
#' @param lmerObj the fitted (g)lmer object used to make predictions
#' @param divide a character vector specifying continuous predictors that should be cut into categorical levels
#' @param n.divide how many categories to use when cutting variables in \code{divide}. For 3, you get -1 SD, Mean,
#'      and +1SD. For 5, you get -2 SD, -1 SD, Mean, +1 SD, +2 SD. Useful for visualizing interactions.
#' @param divide.prefix Whether to include the variable name in naming the cut points
#' @param n.cont The number of points along the continuous predictor range at which to compute an estimate
#' @param cont.pts A list named by predictor where each element is a vector of values at which to compute an estimate
#' @param fixat0 Predictors that should be fixed at 0 in the estimates
#' @param yoked Useful when predictors are transformations of each other (need to document this better).
#'
#' @details
#'
#' This function was adapted from code initially developed by Doug Bates.
#' This is currently part of the predict method of lmer: https://github.com/bbolker/mixedmodels-misc/blob/master/glmmFAQ.rmd
#'
#' @importFrom Hmisc capitalize
#' @importFrom lme4 lmer glmer
#' @return A data.frame containing model-predicted values for each predictor, along with standard errors (se),
#'     95\% CIs (ci_lo and ci_hi), and 95\% prediction intervals (pred_lo and pred_hi).
#'
#' @examples
#'   data(rat.brain)
#'   m <- lmer(activate ~ region * treatment + (1 | animal), data=rat.brain)
#'   pred_vals <- lmer_predict(m)
#'   ggplot(pred_vals, aes(x=region, y=activate, ymin=activate-se, ymax=activate+se, color=treatment)) +
#'     geom_pointrange()
#' 
#' @author Michael Hallquist
#' @export

lmer_predict <- function(lmerObj, divide=NULL, n.divide=3, divide.prefix=TRUE, n.cont=20, cont.pts=NULL, fixat0=NULL, yoked=NULL) { 
  #print cell means for lmer by expanding level combinations and multiplying against fixed effects  
  predNames <- attr(terms(lmerObj), "term.labels")
  
  #whichME <- attr(terms(lmerObj), "order")
  #predNames <- predNames[whichME==1]
  
  #rather than require main effects as above, just get the unique model terms
  #again, a hack for nested models
  predNames <- unique(unlist(strsplit(predNames, ":")))
  
  #sort yoked predictors to the end
  if (!is.null(yoked)) {
    yoked.tf <- as.numeric(predNames %in% sapply(yoked, "[[", "source"))
    predNames <- predNames[rev(order(yoked.tf))]
  }
  
  
  predData <- list()
  #divide into categorical and continuous predictors, determine which continuous predictors to discretize
  for (f in predNames) {
    if (f %in% fixat0) { predData[[f]] <- 0 #compute model prediction when this term is 0
      #} else if (attr(terms(lmerObj), "dataClasses")[f] == "factor") {
    } else if (attr(attr(lmerObj@frame, "terms"), "dataClasses")[f] == "factor") {
      predData[[f]] <- levels(lmerObj@frame[[f]])  
    } else if (attr(attr(lmerObj@frame, "terms"), "dataClasses")[f] == "character") {
      predData[[f]] <- unique(lmerObj@frame[[f]])
    } else {
      if (f %in% divide) {
        #divide into -1 SD, M, + 1 SD; or -2SD, -1SD, M, +1SD, +2SD
        fsd <- sd(lmerObj@frame[[f]], na.rm=TRUE)
        fm <- mean(lmerObj@frame[[f]], na.rm=TRUE)
        predData[[f]] <- if (n.divide==3) { c(fm-fsd, fm, fm+fsd)
            } else { c(fm-fsd*2, fm-fsd, fm, fm+fsd, fm+fsd*2) }
      } else if (!is.null(yoked) && any(pmatch <- which(f %in% sapply(yoked, "[[", "dest") == TRUE))) {
        next #do nothing -- must propagate after expand.grid
      }	else {
        if (!is.null(names(n.cont))) {
          #Named vector specifying number of points to predict for each IV
          if (is.na(n.cont[f])) stop("Cannot locate number of continuous pred points for: ", f)
          predData[[f]] <- seq(min(lmerObj@frame[[f]], na.rm=TRUE), max(lmerObj@frame[[f]], na.rm=TRUE), length=n.cont[f])          
        } else if (!is.null(cont.pts)) {
          #named list for continuous predictor of points at which to evaluate function
          predData[[f]] <- cont.pts[[f]] 
        } else {
          #treat as truly continuous predictor and compute models estimates across the range of observed values
          predData[[f]] <- seq(min(lmerObj@frame[[f]], na.rm=TRUE), max(lmerObj@frame[[f]], na.rm=TRUE), length=n.cont)
        }
      }
    }
  }
  
  #dependent variable
  dvname <- as.character(terms(lmerObj)[[2]])
  
  #populate the model-predicted estimates with 0s prior to building model matrix
  predData[[dvname]] <- 0
  
  #Develop a grid (assumes fully factorial design!)
  predData <- do.call(expand.grid, list(predData))
  
  #handle yoked predictors after the expansion
  if (!is.null(yoked)) {
    for (i in 1:length(yoked)) {
      predData[[yoked[[i]]$dest]] <- sapply(predData[[yoked[[i]]$source]], yoked[[i]]$transform)
    }
  }
  
  mm <- model.matrix(terms(lmerObj),predData)
  
  #drop column names from model matrix where there is no fixed effect represented
  #this will handle nested designs where there are holes in the design
  mm <- mm[,names(fixef(lmerObj))]
  
  #this does not handle the fact that there are many invalid rows in predData though
  
  predData[[dvname]] <- as.vector(mm %*% fixef(lmerObj))
  pvar1 <- diag(mm %*% tcrossprod(as.matrix(vcov(lmerObj)),mm)) #for mysterious reasons, need to cast dpoMatrix from vcov to matrix type
  tvar1 <- pvar1+VarCorr(lmerObj)[[1]][1] #assumes that the first element in VarCorr is subject
  
  #confidence and prediction intervals
  predData <- data.frame(
      predData, se=sqrt(pvar1),
      ci_lo = predData[[dvname]]-1.96*sqrt(pvar1),
      ci_hi = predData[[dvname]]+1.96*sqrt(pvar1),
      pred_lo = predData[[dvname]]-1.96*sqrt(tvar1),
      pred_hi = predData[[dvname]]+1.96*sqrt(tvar1)
  )
  
  for (f in divide) {
    if (n.divide==3) { flevels <- c("-1 SD", "Mean", "+1 SD")
    } else if (n.divide==5) { flevels <- c("-2SD", "-1 SD", "Mean", "+1 SD", "+2 SD") }
    if (divide.prefix) flevels <- paste(Hmisc::capitalize(f), flevels, sep=": ")
    predData[[f]] <- factor(predData[[f]], levels=sort(unique(predData[[f]])), labels=flevels)
  }
  
  return(predData)
}

####FUNCTION FOR SETTING UP EFFECTS TESTS AT EACH TIME
getSimpleEffectsMatrix <- function(coefNames, iv.slice, iv.test, levels.slice, levels.test) {
  require(gtools)
  
  numEffects <- choose(length(levels.test), 2)
  
  conMats <- list()
  for (l in levels.slice) {
    thisCon <- matrix(0, nrow=numEffects, ncol=length(coefNames))
    conNames <- rep(NA_character_, numEffects)
    colnames(thisCon) <- coefNames
    combs <- combinations(length(levels.test), 2)
    for (pair in 1:nrow(combs)) {
      p1 <- levels.test[combs[pair,1]]
      p2 <- levels.test[combs[pair,2]]
      me.p1 <- paste(iv.test, p1, sep="")
      me.p2 <- paste(iv.test, p2, sep="")
      
      #add main effects
      if(any(grepl(paste("^", me.p1, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair,me.p1] <- 1
      if(any(grepl(paste("^", me.p2, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair,me.p2] <- -1
      
      #add interactions
      thisSlice <- paste(iv.slice, l, sep="")
      #allow for either order (depending on how model was specified
      if (any(grepl(paste("^", me.p1, ":", thisSlice, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair, paste(me.p1, ":", thisSlice, sep="")] <- 1
      else if (any(grepl(paste("^", thisSlice, ":", me.p1, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair, paste(thisSlice, ":", me.p1, sep="")] <- 1
      if (any(grepl(paste("^", me.p2, ":", thisSlice, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair, paste(me.p2, ":", thisSlice, sep="")] <- -1
      else if (any(grepl(paste("^", thisSlice, ":", me.p2, "$", sep=""), coefNames, perl=TRUE))) thisCon[pair, paste(thisSlice, ":", me.p2, sep="")] <- -1
      conNames[pair] <- paste(p1, "-", p2, "AT", l)
    }
    rownames(thisCon) <- conNames
    conMats[[l]] <- thisCon
  }
  
  return(conMats)
  
}
