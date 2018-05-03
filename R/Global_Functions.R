#wrapper for running an fsl command safely within R
#if FSL does not have its configuration setup properly, commands such as feat don't work, or hang strangely
runFSLCommand <- function(args, fsldir=NULL, stdout=NULL, stderr=NULL) {
  #look for FSLDIR in system environment if not passed in
  if (is.null(fsldir)) {
    env <- system("env", intern=TRUE)
    if (length(fsldir <- grep("^FSLDIR=", env, value=TRUE)) > 0L) {
      fsldir <- sub("^FSLDIR=", "", fsldir)
    } else {
      warning("FSLDIR not found in environment. Defaulting to /usr/local/fsl.")
      fsldir <- "/usr/local/fsl"
    }
  }
  
  Sys.setenv(FSLDIR=fsldir) #export to R environment
  fslsetup=paste0("FSLDIR=", fsldir, "; PATH=${FSLDIR}/bin:${PATH}; . ${FSLDIR}/etc/fslconf/fsl.sh; ${FSLDIR}/bin/")
  fslcmd=paste0(fslsetup, args)
  if (!is.null(stdout)) { fslcmd=paste(fslcmd, ">", stdout) }
  if (!is.null(stderr)) { fslcmd=paste(fslcmd, "2>", stderr) }
  retcode <- system(fslcmd)
  return(retcode)
}




#setup reshape wide to long
buildWideLongParams <- function(varPrefixes, times) {
  varying <- vector("list", length(varPrefixes))
  for (i in 1:length(varPrefixes)) {
    varying[[i]] <- paste(varPrefixes[i], times, sep="")
  }
  
  v.names <- varPrefixes
  
  return(list(varying=varying, v.names=v.names))
}

varcomp <- function(covmat,n) {
  if (is.list(covmat)) {
    if (length(covmat) < 2)
      stop("covmat must be a list with at least 2 elements")
    ps <- as.vector(sapply(covmat,dim))
    if (sum(ps[1] == ps) != length(ps))
      stop("all covariance matrices must have the same dimension")
    p <- ps[1]
    q <- length(covmat)
    if (length(n) == 1)
      Ng <- rep(n,q)
    else if (length(n) == q)
      Ng <- n
    else
      stop("n must be equal length(covmat) or 1")
    
    DNAME <- deparse(substitute(covmat))
  }
  
  else
    stop("covmat must be a list")
  
  ng <- Ng - 1
  Ag <- lapply(1:length(covmat),function(i,mat,n) { n[i] * mat[[i]] },mat=covmat,n=ng)
  A <- matrix(colSums(matrix(unlist(Ag),ncol=p^2,byrow=T)),ncol=p)
  log.detAg <- sapply(Ag,function(x) determinant(x, logarithm=TRUE)$modulus)
  log.detA <- determinant(A, logarithm=TRUE)$modulus
  log.V1 <- as.numeric( sum((ng/2)*log.detAg) - (sum(ng)/2)*log.detA )
  kg <- ng/sum(ng)
  log.l1 <- (p*sum(ng)/2) * sum( kg*log(1/kg)) + log.V1
  rho <- 1 - (sum(1/ng) - 1/sum(ng))*(2*p^2+3*p-1)/(6*(p+1)*(q-1))
  w2 <- p*(p+1) * ((p-1)*(p+2) * (sum(1/ng^2) - 1/(sum(ng)^2)) - 6*(q-1)*(1-rho)^2) / (48*rho^2)
  f <- 0.5 * (q-1)*p*(p+1)
  STATISTIC <- -2*rho*log.l1
  
#  ng <- Ng - 1
#  Ag <- lapply(1:length(covmat),function(i,mat,n) { n[i] * mat[[i]] },mat=covmat,n=ng)
#  A <- matrix(colSums(matrix(unlist(Ag),ncol=p^2,byrow=T)),ncol=p)
#  browser()
#  detAg <- sapply(Ag,det)
#  detA <- det(A)
#  V1 <- prod(detAg^(ng/2))/(detA^(sum(ng)/2))
#  kg <- ng/sum(ng)
#  l1 <- prod((1/kg)^kg)^(p*sum(ng)/2) * V1
#  rho <- 1 - (sum(1/ng) - 1/sum(ng))*(2*p^2+3*p-1)/(6*(p+1)*(q-1))
#  w2 <- p*(p+1) * ((p-1)*(p+2) * (sum(1/ng^2) - 1/(sum(ng)^2)) - 6*(q-1)*(1-rho)^2) / (48*rho^2)
#  f <- 0.5 * (q-1)*p*(p+1)
#  STATISTIC <- -2*rho*log(l1)
  PVAL <- 1 - (pchisq(STATISTIC,f) + w2*(pchisq(STATISTIC,f+4) - pchisq(STATISTIC,f)))
  names(STATISTIC) <- "corrected lambda*"
  names(f) <- "df"
  RVAL <- structure(list(statistic = STATISTIC, parameter = f,p.value = PVAL, data.name = DNAME, method = "Equality of Covariances Matrices Test"),class="htest")
  return(RVAL)
}

corHeatmap <- function(df, cormat=NULL, alphaSort=TRUE, base_size=14, tileTextSize=3.8) {
  require(reshape2)
  require(ggplot2)
  require(scales)
  require(grid)
  
  if (is.null(cormat)) {
    if (alphaSort) {
      df <- df[,sort(names(df))] #sort variables alphabetically
    }
    
    df <- df[,sapply(df, is.numeric)]
    
    cormat <- cor(df, use="pairwise.complete.obs")
  }
  
  molten.cormat <- melt(cormat)
  names(molten.cormat) <- c("V1", "V2", "corr")
  
  #only retain the lower triangle correlations
  cormat.lower <- molten.cormat[lower.tri(cormat),]
  cormat.upper <- molten.cormat[upper.tri(cormat),]
  
  #create a df with just the diagonal for plotting text
  cormat.ids <- subset(molten.cormat, V1 == V2)
  
  p <- ggplot(cormat.upper, aes(x=V1, y=V2, fill=corr)) + theme_bw(base_size=base_size) + geom_tile() #+ ggtitle(paste("EIFB Intercorrelations for month:", thisMonth))
  p <- p + geom_text(data=cormat.upper, aes(label=round(corr,2)), size=tileTextSize)
  p <- p + geom_text(data=cormat.ids, aes(x=V1, label=V1), colour="grey30", show.legend=FALSE, size=tileTextSize, hjust=0.13, vjust=0.5, angle=0)
  p <- p + scale_fill_gradient2(name="Correlation\n", low=muted("blue"), high=muted("red"))
  #p <- p + scale_fill_gradientn(name="Correlation", colours= c("white", "red"), limits=c(0,1))
  
  #want matrix to run from upper left to lower right, so need to change limits of axes
  facLevels <- sort(as.character(unique(molten.cormat$V1)))
  
  p <- p + scale_x_discrete(name="", limits=facLevels, labels=waiver()) + scale_y_discrete(name="", limits=rev(facLevels), labels=waiver()) + xlab("") + ylab("") +
      theme(legend.position=c(0.8, 0.8), plot.margin=unit(c(0.2,0.2,0.2,0.2), "lines"), axis.title.x=element_blank(), axis.title.y=element_blank()) +
      coord_cartesian(xlim=c(min(as.numeric(cormat.upper$V1)) - 0.5, max(as.numeric(cormat.upper$V1) + 3))) #pad a bit on the right
  #ylim=c(min(as.numeric(cormat.upper$V2)) - 1.5, max(as.numeric(cormat.upper$V2) + 3)))
  
  p <- p + theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
  return(p)
}


#function swiped from http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html
corstarsl <- function(x, omit){
  require(Hmisc)
  if (!missing(omit)) x <- x[,!names(x) %in% omit]
  
  #remove categorical data for now (generates problems
  x <- x[,sapply(x, is.numeric)]
  
  x <- as.matrix(x)
  
  
  #need to specify Hmisc because ggm also has an rcorr that we don't want!
  R <- Hmisc::rcorr(x)$r
  p <- Hmisc::rcorr(x)$P
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
  
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  
  #if column names would exceed line width, convert to numbers corresponding to rows
  lineWidth <- getOption("width")
  if (nchar(paste(colnames(x), sep=" ", collapse=" ")) > lineWidth)  {
    rownames(Rnew) <- paste(1:length(colnames(x)), "-", colnames(x))
    colnames(Rnew) <- as.character(1:length(colnames(x)))
  }
  else {
    rownames(Rnew) <- colnames(x)
    #colnames(Rnew) <- paste(colnames(x), "", sep="")    
    colnames(Rnew) <- colnames(x) #other person's code used paste above... To convert to character?? Unclear.
  }
  
  
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
#  name.width <- max(sapply(names(Rnew), nchar))
#  names(Rnew) <- format(names(Rnew), width = name.width, justify = "left")
#  
#	return(format(Rnew, width = name.width, justify = "left"))
  return(Rnew)
}

#pretty redundant with describeOneVar below, but tweaked to provide return value as data.frame
getDescriptivesByGroup <- function(df, outcomes, group, zscale=FALSE, tscale=FALSE) {
  require(plotrix)
  oneVar <- function(df, outcome) {
    stats <- c("mean", "sd", "std.error", "median", "min", "max")
    bigMatrix <- c()
    
    for (stat in stats) {
      if (zscale) {
        bigMatrix <- cbind(bigMatrix, tapply(as.vector(scale(df[[outcome]])), df[[group]], stat, na.rm=TRUE))  
      } else if (tscale) {
        scaleVec <- as.vector(scale(df[[outcome]]))
        scaleVec <- 50 + 10*scaleVec
        bigMatrix <- cbind(bigMatrix, tapply(scaleVec, df[[group]], stat, na.rm=TRUE))
      } else {
        bigMatrix <- cbind(bigMatrix, tapply(df[[outcome]], df[[group]], stat, na.rm=TRUE))
      }
      
      colnames(bigMatrix)[ncol(bigMatrix)] <- stat
    }
    
    bigMatrix <- as.data.frame(bigMatrix)
    bigMatrix$var <- outcome
    bigMatrix$group <- factor(rownames(bigMatrix))
    rownames(bigMatrix) <- NULL
    
    return(bigMatrix)    
  }
  
  allVars <- c()
  for (outcome in outcomes) {
    allVars <- rbind(allVars, oneVar(df, outcome))
  }
  allVars <- plyr::rename(allVars, c(group=group))
  return(allVars)
}

describeBy <- function(df, outcomes, group) {
  require(plotrix)
  describeOneVar <- function(df, outcome, group) {
    #klunky handling of group presence v. absence
    if (missing(group)) {
      #handle cases where no grouping variable is given
      #categorical variable gets different treatment
      if (is.factor(df[[outcome]])) {
        cat("\nTable of", outcome, "counts\n")
        print(.xtable <- xtabs(as.formula(paste("~", outcome)), data=df))
        cat("\nTable of", outcome, "proportions\n")
        print(round(prop.table(.xtable), 2))
        cat("\nChi-Square test\n")
        print(chisq.test(.xtable))
        cat("\nChi-Square test using simulated p values from bootstrap resampling (B=10000)\n")
        print(chisq.test(.xtable,simulate.p.value=TRUE,B=10000))
      }
      else {
        stats <- c("mean", "sd", "std.error", "median", "min", "max")
        bigMatrix <- c()
        
        for (stat in stats) {
          #bigMatrix <- rbind(bigMatrix, sapply(df[[outcome]], stat, na.rm=TRUE))
          bigMatrix <- rbind(bigMatrix, do.call(stat, list(df[[outcome]], na.rm=TRUE))) 
          #    sapply(df[[outcome]], stat, na.rm=TRUE))
          rownames(bigMatrix)[nrow(bigMatrix)] <- stat
        }
        cat("\nDescriptive Statistics for", outcome, "\n")
        print(bigMatrix)
      }	
      
    }
    else {
      #categorical variable gets different treatment
      if (is.factor(df[[outcome]])) {
        cat("\nTable of", outcome, "counts per", group, "\n")
        print(.xtable <- xtabs(as.formula(paste("~", outcome, "+", group)), data=df))
        cat("\nTable of", outcome, "proportions per", group, "\n")
        print(round(prop.table(.xtable, 2), 2))
        cat("\nChi-Square test\n")
        print(chisq.test(.xtable))
        cat("\nChi-Square test using simulated p values from bootstrap resampling (B=10000)\n")
        print(chisq.test(.xtable,simulate.p.value=TRUE,B=10000))
      }
      else {
        stats <- c("mean", "sd", "std.error", "median", "min", "max")
        bigMatrix <- c()
        
        for (stat in stats) {
          bigMatrix <- rbind(bigMatrix, tapply(df[[outcome]], df[,group], stat, na.rm=TRUE))
          rownames(bigMatrix)[nrow(bigMatrix)] <- stat
        }
        cat("\nDescriptive Statistics for", outcome, "counts per", group, "\n")
        print(bigMatrix)
        return(bigMatrix)
      }
    }
  }	
  
  for (outcome in outcomes) {
    cat("\n##################\nOutput for: ", outcome, "\n\n")
    describeOneVar(df, outcome, group)
  }
  
}

oneWayANOVA <- function(dataset, outcome, group) {
  require(gplots)
  aovformula <- as.formula(paste(outcome, "~", group))
  plotmeans(aovformula, data=dataset)
  
  cat("\n\n-----\nGroup means and SDs\n\n")
  meanSd <- tapply(dataset[[outcome]], dataset[[group]], mean, na.rm=TRUE)
  meanSd <- rbind(meanSd, tapply(dataset[[outcome]], dataset[[group]], sd, na.rm=TRUE)) 
  row.names(meanSd) <- c("Mean", "SD")
  print(meanSd)
  
  cat("\n\n-----\nOne-way ANOVA\n")
  print(summary(anovaModel <- aov(aovformula, dataset)))
  
#  cat("\n\n-----Planned contrasts\n")
#  cat("\nAll with class 1\n")
#  #setup planned contrasts here (fit.contrasts requires one to test fewer contrasts than the num factor levels)
#  plannedContrasts <- rbind( "1 vs 2" = c(-1, 1, 0, 0, 0),
#      "1 vs 3" = c(-1, 0, 1, 0, 0),
#      "1 vs 4" = c(-1, 0, 0, 1, 0),
#      "1 vs 5" = c(-1, 0, 0, 0, 1))
#  
#  print(fit.contrast(anovaModel, group, plannedContrasts))
  
#  cat("\n3v5, 2v4\n")		
#  plannedContrasts <- rbind( "3 vs 5" = c(0, 0, -1, 0, 1),
#      "2 vs 4" = c(0, -1, 0, 1, 0))
#  
#  print(fit.contrast(anovaModel, group, plannedContrasts))
  
  cat("\n\n-----\nTukey's HSD Post Hocs\n")
  print(TukeyHSD(anovaModel, ordered=FALSE))
}



missingDataReport <- function(dataset, varnames, idVars="id", idsToCheck, outputOptions, iqrmultiplier=2) {
  require(plyr)
  require(grid)
  require(gridExtra)
  if (!missing(outputOptions)) outputOptions <- strsplit(outputOptions, "\\s+", perl=TRUE)[[1]]
  if (missing(varnames) || length(varnames) < 1) varnames <- names(dataset) #if no variable names specified, display all
  
  if (missing(idsToCheck)) workingDF <- dataset[,c(idVars, varnames)]
  else workingDF <- dataset[idsToCheck, c(idVars, varnames)]
  
  missingDetail <- FALSE
  missingSummary <- TRUE
  
  #in the case of multiple ID variable, generate a single variable to print out which IDs are missing in summary
  workingDF$idcomb <- apply(workingDF[,idVars, drop=FALSE], 1, function(row) { paste(as.character(row), collapse=".") })
  
  if (missingSummary == TRUE) {
    cat("Missing Summary Report\n----------------------\n\n")
    missSummary <- ldply(varnames, function(thisVar) {
          dataToCheck <- workingDF[[thisVar]]
          numMissing <- sum(is.na(dataToCheck))
          numNonMissing <- length(dataToCheck) - numMissing          
          missingIDs <- paste(strwrap(paste(workingDF$idcomb[which(is.na(dataToCheck))], collapse=", "), width=50, indent=0, exdent=4), collapse="\n") 
          #if (length(missingIDs)==0) { missingIDs <- "" } else { browser() }
          #if (missingIDs=="") { missingIDs <- "" } else { browser() }
          return(data.frame(varName=thisVar, numMissing=numMissing, numNonMissing=numNonMissing, missingIDs=missingIDs))
        })
    
    print(missSummary, row.names=FALSE)
    
    #http://stackoverflow.com/questions/15937131/print-to-pdf-file-using-grid-table-in-r-too-many-rows-to-fit-on-one-page
    tg <- tableGrob(missSummary, rows = seq_len(nrow(missSummary))) 
    fullheight <- convertHeight(sum(tg$heights), "cm", valueOnly = TRUE)
    margin <- convertHeight(unit(0.51,"in"), "cm", valueOnly = TRUE)
    a4height <- 29.7 - margin
    nrows <- nrow(tg)
    npages <- ceiling(fullheight / a4height)
    
    heights <- convertHeight(tg$heights, "cm", valueOnly = TRUE) 
    rows <- cut(cumsum(heights), include.lowest = FALSE,
        breaks = c(0, cumsum(rep(a4height, npages))))
    
    groups <- split(seq_len(nrows), rows)
    
    gl <- lapply(groups, function(id) tg[id,])
    
    #pdf("multipage.pdf", paper = "a4", width = 0, height = 0)
    pdf("multipage.pdf", width = 9, height = 15)
    for(page in seq_len(npages)){
      grid.newpage()
      #grid.rect(width=unit(21,"cm") - unit(0.51,"in"),
      #    height=unit(29.7,"cm")- unit(0.51,"in"))
      grid.draw(gl[[page]])
    }
    ## alternative to explicit loop:
    ## print(marrangeGrob(grobs=gl, ncol=1, nrow=1, top=NULL))
    dev.off()
    
    
    #grid.newpage()
    #grid.table(missSummary)
  }
  
  
  if (missingDetail == TRUE) {
    cat("\nMissing Detail Report\n-------------------------\n\n")
    d_ply(workingDF, idVars, function(observation) {
          #is.na function will naturally check all columns and rows for a data.frame, handles different data types
          numMissing <- sum(is.na(observation))
          if (numMissing == 0) return(NULL)
          cat("--------------------------\n")
          #create a single line with the names of the id vars and values for this person
          valString <- paste(laply(idVars, function(var) paste(var, observation[[var]], sep=": ")), collapse=", ")
          cat(valString, "\n")
          cat("Variables missing:", numMissing, "\n")
          missingVars <- data.frame(varname=names(observation)[is.na(observation)])
          print(missingVars)
        })
  }
  
  
  
#	%let dupprint = 0;
#			%let dupcheck = 0;
#			%let missingsummary = 0;
#			%let lowhighsummary = 1;
#			%let missingdetail = 0;
#			%let outliercheck = 0;
}



#help for tom booth
#tomFunction <- function() {
#	library(polycor)
#	exampleHetCorr <- data.frame(
#			age=c(15, 21, 60, 25, 33),
#			income=c(0, 5000, 20000, 10000, 15000),
#			sex=factor(c("male", "female", "male", "female", "male")),
#			q1=factor(c("1-not at all", "2-slightly", "3-very much", "2-slightly", "1-not at all"), ordered=TRUE),
#			q2=factor(c("false", "true", "true", "false", "true"))
#			)
#			
#	hetcorObject <- hetcor(exampleHetCorr, ML=TRUE, std.err=TRUE)
#	print(hetcorObject)
#	
#	corrsOnly <- hetcorObject$correlations
#
#	sdVector <- sapply(exampleHetCorr, sd, na.rm=TRUE)
#	
#	#sort correlations for each row
#	apply(corrsOnly, 1, sort)
#	
#	#if you need to retain the names for the pairings, try melt
#	#and ddply
#	library(reshape)
#	meltedCorrs <- melt(corrsOnly)
#	
#	library(plyr)
#	sortedPairs <- ddply(meltedCorrs, "X1", function(oneVarCorrs) {
#				#use the order function to sort the one-variable data.frame by correlation value
#				return(oneVarCorrs[order(oneVarCorrs$value),])
#			})
#	
#	print(sortedPairs)
#	
#	factanal(covmat=corrsOnly, factors=2, n.obs=362, rotation="promax")
#}


dbGetEIFBConnection <- function() {
  
  #fail over to local database if X unavailable
  #for whatever reason, with the latest ODBC drivers, we need odbcConnectAccess2007 when on 64-bit R
  #and odbcConnectAccess when on 32-bit R. The connection will fail otherwise.
  
  if (.Platform$OS.type == "unix") {
    #odbc irrelevant, use local sqlite copy
    require(RSQLite)
    EIFBDB <- dbConnect(dbDriver("SQLite"), dbname=file.path(getMainDir(), "Miscellaneous", "Shared_Data", "EIFB_Database_22Feb2012.sqlite"))
  }
  else {
    require(RODBC)
    if (sessionInfo()$R.version$arch == "i386") {
      if((EIFBDB <- odbcConnectAccess("X:/Data/Emotion IFB/emotion IFB database.mdb", pwd="datateam1")) < 0)
        EIFBDB <- odbcConnectAccess(file.path(getMainDir(), "Miscellaneous", "Shared_Data", "EIFB_Database_8Mar2011.mdb"))  
    }
    else if (sessionInfo()$R.version$arch == "x86_64") {
      if((EIFBDB <- odbcConnectAccess2007("X:/Data/Emotion IFB/emotion IFB database.mdb", pwd="datateam1")) < 0)
        EIFBDB <- odbcConnectAccess2007(file.path(getMainDir(), "Miscellaneous", "Shared_Data", "EIFB_Database_8Mar2011.mdb"))
    }
  }
  
  return(EIFBDB)
}

dbGetIFBConnection <- function() {
  
  #fail over to local database if X unavailable
  #for whatever reason, with the latest ODBC drivers, we need odbcConnectAccess2007 when on 64-bit R
  #and odbcConnectAccess when on 32-bit R. The connection will fail otherwise.
  
  if (.Platform$OS.type == "unix") {
    #odbc irrelevant, use local sqlite copy
    require(RSQLite)
    IFBDB <- dbConnect(dbDriver("SQLite"), dbname=file.path(getMainDir(), "Miscellaneous", "Shared_Data", "IFB_Database_22Jul2010.sqlite"))
  }
  else {
    require(RODBC)
    if (sessionInfo()$R.version$arch == "i386") {
      if((IFBDB <- odbcConnectAccess("X:/Data/Emotion IFB/emotion IFB database.mdb", pwd="datateam1")) < 0)
        IFBDB <- odbcConnectAccess("C:/Data_Analysis/EIFB_Psychophys/emotion IFB database 22Feb2010.mdb", pwd="datateam1")  
    }
    else if (sessionInfo()$R.version$arch == "x86_64") {
      if((IFBDB <- odbcConnectAccess2007("X:/Data/Emotion IFB/emotion IFB database.mdb", pwd="datateam1")) < 0)
        IFBDB <- odbcConnectAccess2007("C:/Data_Analysis/EIFB_Psychophys/emotion IFB database 22Feb2010.mdb", pwd="datateam1")
    }
  }
  
  return(IFBDB)
}

dbGetGregDBConnection <- function() {
  #fail over to local database if X unavailable
  #require(RODBC)
  require(RSQLite)
  if (.Platform$OS.type == "unix")
    gregDB <- dbConnect(dbDriver("SQLite"), dbname=file.path(getMainDir(), "Miscellaneous", "Shared_Data", "GregEIFB_DB_1Aug2012.sqlite"))
  else if (sessionInfo()$R.version$arch == "i386") {
    if((gregDB <- odbcConnectAccess("//oacres3/rcn/pican/DATABASES/bpddb.mdb")) < 0)
      gregDB <- odbcConnectAccess(file.path(getMainDir(), "Miscellaneous", "Shared_Data", "GregEIFB_DB_8Mar2011.mdb"))    
  }
  else if (sessionInfo()$R.version$arch == "x86_64") {
    if((gregDB <- odbcConnectAccess2007("//oacres3/rcn/pican/DATABASES/bpddb.mdb")) < 0)
      gregDB <- odbcConnectAccess2007(file.path(getMainDir(), "Miscellaneous", "Shared_Data", "GregEIFB_DB_8Mar2011.mdb"))
  }
  
  return(gregDB)
}

f_centerPredictors <- function(df, predictors, scale=FALSE, addsuffix=NULL) {
  for (predictor in predictors) {
    if (!is.null(addsuffix)) { predout <- paste0(predictor, addsuffix) } else { predout <- predictor }
    if (!grepl(":", predictor, fixed=TRUE) && is.numeric(df[[predictor]])) df[[predout]] <- as.vector(scale(df[[predictor]], center=TRUE, scale=scale))
  }
  return(df)
}

estimateLME <- function(df, outcome, predictors=NULL, timevar=NULL, idvar, modelOrder, centerPredictors=FALSE) {
  require(nlme)
  
  #subfunction for building model formula
  buildFormula <- function(outcome, timeVar, predictors, modelOrder) {
    formString <- paste(outcome, " ~ 1", sep="")
    
    if (modelOrder > 0) formString <- paste(formString, " + ", timeVar, sep="")
    if (modelOrder > 1) formString <- paste(formString, " + I(", timeVar, "^2)", sep="")
    if (modelOrder > 2) formString <- paste(formString, " + I(", timeVar, "^3)", sep="")
    
    for(predictor in predictors) {
      #add initial status effect
      formString <- paste(formString, " + ", predictor, sep="")
      
      #add linear rate of change effect 
      if (modelOrder > 0) formString <- paste(formString, " + ", timeVar, ":", predictor, sep="")
      if (modelOrder > 1) formString <- paste(formString, " + I(", timeVar, "^2):", predictor, sep="")
      if (modelOrder > 2) formString <- paste(formString, " + I(", timeVar, "^3):", predictor, sep="")
      
    }
    return(formula(formString))
    
  }
  
  if (centerPredictors == TRUE && !is.null(predictors)) df <- f_centerPredictors(df, predictors)
  
  lmeModel <- NULL
  fixedFormula <- buildFormula(outcome, timevar, predictors, modelOrder)
  if (is.null(timevar)) randomFormula <- formula(paste("~1|", idvar))
  else randomFormula <- formula(paste("~", timevar, "|", idvar))
  
  #fairly psychotic solution to the issue of needing lmeFormMeans for the ANOVA call and getting an object not found (not part of local environment)
  #see this: http://finzi.psych.upenn.edu/R/Rhelp02a/archive/16599.html
  #print(summary(uncMeansModel <- eval(substitute(lme(lmeFormMeans, data=outcomeLongDF, random=~1|studyid, method="ML", na.action=na.exclude, control=lmeControl(maxIter=200, msMaxIter=200, opt="optim")), list(lmeFormMeans=lmeFormMeans)))))
  
  tryCatch(lmeModel <- eval(substitute(lme(fixedFormula, data=df, random=randomFormula, method="ML", na.action=na.exclude, 
                  control=lmeControl(maxIter=500, msMaxIter=500, opt="optim")), 
              list(fixedFormula=fixedFormula, randomFormula=randomFormula))), error = function(err) {
        
        cat("Error running LME of form: ", as.character(fixedFormula), ".\n\nError: ", err$message, "\n", sep="")
        lmeModel <<- NULL
      })
  return(lmeModel)
  
}

computeTrajectory <- function(df, b_vec, predictors, timevar, sdmults=c(-1, 0, 1), assumeCentered=TRUE) {
  recursePredictor <- function(df, b_vec, predictors, pos, sdmults, m, sd, continuous=TRUE) {
    thisPredictor <- predictors[pos]
    if (continuous) {
      for (mult in sdmults) {
        mePred <- b_vec[thisPredictor]*mult #effect of predictor on initial status (main effect)
        meTimeVar <- grep(paste("(", thisPredictor, ":", timevar, "|", timevar, ":", thisPredictor, ")", sep=""), names(b_vec), perl=T)
        meTime <- b_vec[meTimeVar]
        
        #look for between-predictor interactions (only at bottom level, I think)
        
      }
    }
  }
  
  continuousPredictors <- names(predictors)[which(sapply(df[,predictors], is.numeric))]
  categoricalPredictors <- names(predictors)[which(sapply(df[,predictors], is.factor))]
  
  pred_m_sd <- matrix(nrow=length(continuousPredictors), ncol=2, dimnames=list(continuousPredictors, c("m", "sd")))
  
  
  timeScores <- sort(unique(df[[timevar]]))
  
  for (predictor in predictors) {
    pred_m_sd[predictor,"m"] <- mean(df[[predictor]], na.rm=TRUE)
    pred_m_sd[predictor,"sd"] <- sd(df[[predictor]], na.rm=TRUE)
    
  }
  
  numCombinations <- length(sdmults)^length(continuousPredictors)
  
  initialStatus <- df[["(Intercept)"]]
  rateOfChange <- df[[timevar]]
  
}

showMemoryUse <- function(sort="size", decreasing=FALSE, limit) {
  objectList <- ls(parent.frame())
  oneKB <- 1024
  oneMB <- 1048576
  oneGB <- 1073741824
  
  memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=paste0('`', x, '`'))))))
  
  memListing <- sapply(memoryUse, function(size) {
        if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
        else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
        else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
        else return(paste(size, "bytes"))
      })
  
  memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
  
  if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
  else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
  
  if(!missing(limit)) memListing <- memListing[1:limit,]
  
  print(memListing, row.names=FALSE)
  return(invisible(memListing))
}

