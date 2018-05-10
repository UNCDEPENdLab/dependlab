#' Create fmri design matrix for AFNI, FSL, or internal convolved design.
#' @param events BDM expects a single data frame that includes a column for the event type (e.g. outcome vs. cue), run number (1:n), trial number (nested within run, 1:x), onset, duration of event
#' @param signals BDM expects a list of list. The first level of lists reflects each signal (e.g. pe vs values). In the second level, BDM expects values and event (e.g. cue vs outcome). Values is a data frame with run number (1:n), trial (1:x), and signal.
#' @param normalizations expects a list that specifies normalization for each run. If not specified, will set to none for each run
#' @param center_values Specified whether to center parameteric regressors
#' @param baselineCoefOrder If set >= 0, then accounting for polynomial trends wihin run in the data (e.g. baselineCoefOrder = 1 includes both an intercept and a linear trend as regressors)
#' @param baselineParameterization Defaults to legendre. If not default, then assumes polynomials are orthogonal to design matrix (not recommended)
#' @param runVolumes Expects a vector of number of Volumes per run, a list of Nifti files to read in (and then BDM calculates number of volumes), or estimates number of runs manually (by computing the (max onset + duration + itipost)/tr within each run)
#' @param dropVolumes By default, assumes all Volumes should be retained. If specified, then expects number of specific volumes that need to be dropped
#' @param runsToOutput
#' @param plot By default, will plot the design matrix in the plot window of your R session. If false, will still provide you the ggplot object in the output structure from BDM
#' @param writeTimingFiles By default, won't write timing files. Accepts a character vector that specifies whether writing AFNI, FSl or preconvolved timing files
#' @param output_directory Where to output the timing files. Note that by default will output the data files to a folder called "run_timing" in the current working directory. If such a folder does not exist, it will make a folder in your current directory.
#' @param tr By default set to 1 second. Can specify TR to be what the tr is during your study (in seconds)
#' @param convolve By default, true. If true, will convolve signals with HRF to get predicted BOLD response for that regressor for each voxel
#' @param convolve_wi_run By default, true. If true, mean centers parameteric regressors within each run.
#' @param add_derivs By default, false. If true, will add temporal derivatives for column after convolution
#' @param parmax1  By default, false. If true, rescales a convolved regressor to max=1.0 after convolution (normalize scaling across runs and subjects)
#' @param high_pass By default, null. Specifies the high pass filter, if desired, to account for scanner drift and other low frequency oscillations in signal that are not attributable to brain activity
#' @param itipost By default, 12. Assumes 12 volumes after each run. Only necessary to specify if not supplying runVolumes and expecting function to use events information to calculate runVolumes
#' @param nuisance_regressors By default, null. If nuisance regressors specified, either expects list of character strings for different .txt files for the nuisance regressors OR it expects a list of data frames (1 df per run). These values are tacked onto design.convolve (and not convolved with HRF), so each regressor should be length of the number of runVolumes within that run.
#' @details
#'
#' This function was adapted from the fitclock package (https://github.com/PennStateDEPENdLab/fitclock.git) to incorporate a broader array of data when building design matrices for fMRI analysis.
#'
#' @return A list of important information regarding the design matrix.
#'        The first element is the pre-convolved design matrix with just the specified events and signals set up as signal*run list with a matrix in each element of the list. Within each matrix, the onset, duration and value of the signal is specified.
#'        The second element is the convolved design matrices for each run. Thus, each element in the list refers to the run. Within each design matrix, there's a column for each regressor (manually specified and additional regressors--e.g. nuisance regressors, polynomial regressors). Each row reflects the predicted value for each volume.
#'        The third element is a list with information about collinearity of pre-convolved regressors. At the highest level of the list, each element specifies the run. At the second level of the list, the first elemnt specifies the correlation between the regressors and the second elemnt provides the VIF associated with each regressor
#'        The fourth element is a list with information about collinearity of convolved regressors (including both particular signals, nuisance regressors, polynomial regressors). Follows the same structure as above
#'        The fifth element is a list with an element for each signal. Within each signal, a vector containing a list of all the onset times concatonated together across runs
#'        The sixth element is vector that specifies the number of Volumes analyzed per run
#'        The seventh element is the ggplot object that is the output from the visualizeDesignMatrix call.
#' @importFrom data.table as.data.table
#' @importFrom plyr round_any
#' @importFrom dplyr filter select slice bind_rows
#' @importFrom orthopolynom legendre.polynomials polynomial.values
#' @importFrom oro.nifti readNIfTI
#' @importFrom ggplot2 ggplot
#' @import fmri_utility_fx
#' @examples {
#' set.seed(480929)
#' events <- dplyr::bind_rows(data.frame(event="cue", rbind(data.frame(run=1,trial=1:50,onset=cumsum(rpois(50, lambda = 2)),duration=rep(2, 50), custom_dur=abs(rnorm(50))),
#'  data.frame(run=2,trial=1:50,onset=cumsum(rpois(50, lambda = 2)),duration=rep(2, 50), custom_dur=abs(rnorm(50))))),
#'  data.frame(event="outcome",rbind(data.frame(run=1,trial=1:50,onset=cumsum(rpois(50, lambda = 2)),duration=rep(2, 50)),
#'             data.frame(run=2,trial=1:50,onset=cumsum(rpois(50, lambda = 2)),duration=rep(2, 50)))))
#'  signals <- list(ev=list(values=rbind(data.frame(run=1, trial=1:50, signal=rnorm(50)), data.frame(run=2, trial=1:50, signal=rnorm(50))), event="cue", duration="custom_dur"),pe=list(values=rbind(data.frame(run=1, trial=1:50, signal=rnorm(50)), data.frame(run=2, trial=1:50, signal=rnorm(50))), event="outcome", duration=1))
#' nuisrun1 <- data.frame(csf = rnorm(100), wm = rnorm(100))
#' nuisrun2 <- data.frame(csf = rnorm(121), wm = rnorm(121))
#' d <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none")) #convolved design matrix
#' dnocon <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none"), convolve = FALSE) #pre-convolved design matrix
#' dnuis <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none"), nuisance_regressors = list(nuisrun1, nuisrun2), baselineCoefOrder = 2) #convolved design matrix with polort 2 and nuisance regressors
#' }
#' @export

#Example Data Set that can be used to visualize what build design matrix expects
# source("fmri_utility_fx.R")
# set.seed(480929)
# events <- dplyr::bind_rows(data.frame(event="cue",
#   rbind(
#     data.frame(
#       run=1,
#       trial=1:50,
#       onset=cumsum(rpois(50, lambda = 2)),
#       duration=rep(2, 50), custom_dur=abs(rnorm(50))),
#     data.frame(
#       run=2,
#       trial=1:50,
#       onset=cumsum(rpois(50, lambda = 2)),
#       duration=rep(2, 50), custom_dur=abs(rnorm(50)))
#   )),
#   data.frame(event="outcome",
#              rbind(
#     data.frame(
#       run=1,
#       trial=1:50,
#       onset=cumsum(rpois(50, lambda = 2)),
#       duration=rep(2, 50)),
#     data.frame(
#       run=2,
#       trial=1:50,
#       onset=cumsum(rpois(50, lambda = 2)),
#       duration=rep(2, 50))
#   ))
# )
#
# signals <- list(
#   ev=list(values=rbind(data.frame(run=1, trial=1:50, signal=rnorm(50)), data.frame(run=2, trial=1:50, signal=rnorm(50))), event="cue", duration="custom_dur"),
#   pe=list(values=rbind(data.frame(run=1, trial=1:50, signal=rnorm(50)), data.frame(run=2, trial=1:50, signal=rnorm(50))), event="outcome", duration=1)
# )
#
#
#
# df1 <- data.frame(csf = rnorm(100), wm = rnorm(100))
# df2 <- data.frame(csf = rnorm(121), wm = rnorm(121))
# #For events, BDM expects a single data frame that includes a column for the event type (e.g. outcome vs. cue), run number (1:n), trial number (nested within run, 1:x), onset, duration of event
# #potential custom duration if want to specify specific lengths of events
# # For signals, BDM expects a list of list. The first level of lists reflects each signal (e.g. pe vs values). In the second level, BDM expects values and event (e.g. cue vs outcome).
# #Values is a data frame with run number (1:n), trial (1:x), and signal.
# #Additionally will also accept a third element to the list which can specify custom durations
# #Also BDM expects either a character vector of the function nifit for each run OR it expects a numerical vector for the volumes to be analyzed in each run
# #If you don't supply the runVolumes, character string it will infer the number of volumes based off of the last onset within the run and will add 12 ITIs to that by default (very risk; do not recommend!)
#
# #Test Case
#
# d <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none"))
# dnocon <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none"), convolve = FALSE)
# dcon <- d$design.convolve$run1
# dcon$time <- 1:nrow(dcon)
# ggplot(dcon, aes(x = time, y = pe)) + geom_line(size = 0.8)
# dnoconout <- dnocon$design.convolve$run1
# dnoconout$time <- 1:nrow(dcon)
# ggplot(dnoconout, aes(x = time, y = pe)) + geom_line(size = 0.8)
# dnuis <- build_design_matrix(events = events, signals = signals, normalizations = c("none", "none"), nuisance_regressors = list(df1, df2), writeTimingFiles = c("AFNI", "FSL", "convolved"), baselineCoefOrder = 2)
# dnuisrun1 <- dnuis$design.convolve$run1
# dnuisrun1$time <- 1:nrow(dnuisrun1)
# ggplot(dnuisrun1, aes(x = time, y = wm)) + geom_line(size = 0.8)

build_design_matrix=function(
  events = NULL,
  signals = NULL,
  normalizations=NULL, #normalization of HRF
  center_values=FALSE, #whether to center parametric regressors prior to convolution
  baselineCoefOrder=-1L,
  baselineParameterization="Legendre",
  runVolumes=NULL, #vector of total fMRI volumes for each run (used for convolved regressors)
  dropVolumes=0L, #vector of how many volumes to drop from the beginning of a given run
  runsToOutput=NULL,
  plot=TRUE,
  writeTimingFiles=NULL,
  output_directory="run_timing",
  tr=1.0, #TR of scan in seconds
  convolve=TRUE, #whether to convolve the regressors with the HRF
  convolve_wi_run=TRUE, #whether to mean center parametric regressors within runs before convolution
  add_derivs=FALSE, #whether to add temporal derivatives for each column after convolution
  parmax1=FALSE, #whether to rescale a convolved regressor to max=1.0 after convolution (normalize scaling across runs and subjects)
  high_pass=NULL, #whether to apply a high-pass filter to the design matrix (e.g., to match fmri preprocessing)
  itipost = 12,
  nuisance_regressors = NULL #allow for nuisance regression text file to be implemented. need separate file for each
  ) {


   signals_aligned <- lapply(signals, function(s) {
    if (is.null(s$event)) { stop("Signal does not have event attribute") }
    df_events <- dplyr::filter(events, event==s$event)
    df_signal <- dplyr::filter(s$values)
    s_aligned <- df_events %>% dplyr::left_join(df_signal, by=c("run", "trial"))
    if (length(s$duration) > 1L) { stop("Don't know how to interpret multi-element duration argument for signal: ", paste0(s$duration, collapse=",")) }
    if(!is.null(s$duration)) {
      if (is.numeric(s$duration)) {
        s_aligned$duration <- s$duration #replicate the scalar on all rows
      } else {
        s_aligned$duration <- s_aligned[[s$duration]]
      }
    }

    #transform to make dmat happy (runs x regressors 2-d list)
    retdf <- s_aligned %>% select(onset, duration, signal) %>% dplyr::rename(value=signal)
    retsplit <- split(retdf, s_aligned$run)
    names(retsplit) <- paste0("run", names(retsplit))
    return(retsplit)
  })

  dmat <- do.call(cbind, lapply(1:length(signals_aligned), function(signal) {
    lapply(signals_aligned[[signal]], function(run) { as.matrix(run) })
  }))

  #define numruns based off of dmat
  nruns = dim(dmat)[[1]]
  if (is.null(runVolumes)) {
    #determine the last fMRI volume to be analyzed
    runVolumes <- rep(0, nruns)
    for(i in 1:nruns) {
      for (j in 1:length(signals_aligned)) {
        #signals_aligned[[i]][[j]]

        currentdf <- signals_aligned[[i]][[j]]
        highesttime <- ceiling(max(currentdf$onset + currentdf$duration + itipost)/tr)
        if (highesttime > runVolumes[i]) { runVolumes[i] <- highesttime } #update runVolumes for ith run only if this signal has later event

        #position = length(currentdf[[1]])
        #runVolumes <- c(runVolumes, (ceiling(currentdf$onset[[position]]+ currentdf$duration[[position]] + itipost)/tr))
        #maxevt_persignal = ceiling()
        }
    }
    message("Assuming that last fMRI volume was 12 seconds after the onset of the last ITI.")
    message(paste0("Resulting lengths: ", paste(runVolumes, collapse=", ")))

    }else if(is.character(runVolumes)) {
      runVolumes <- c()
      for( i in 1:length(runVolumes)) {
        currentNifti <- runVolumes[[i]]
        out <- readNIfTI(currentNifti, read_data = FALSE)
        runVolume <- dim(out@.Data)[[4]] # grabs the 4th dimension of the matrix, which is the number of Volumes
        runVolumes <- c(runVolumes, runVolume)
      }
    } else {
      runVolumes = runVolumes
    }
    if (is.character(nuisance_regressors)) {
    nuisance_regressors_df <- data.frame()
    for(i in 1:length(nuisance_regressors)) {
      nuisance_regressor_currun <- read.table(nuisance_regressors[[i]])
      nuisance_regressors_currun$run <- i
      runVolume = runVolumes[[i]]
      print(runVolume)
      nuisance_regressors_currun <- dplyr::slice(nuisance_regressor_currun, 1:runVolume) %>% as.data.frame()
      nuisance_regressors_df <- bind_rows(nuisance_regressors_df, nuisance_regressor_currun)}
    } else if (!is.null(nuisance_regressors)) {
      #assuming that a list of dataframes for each run of the data
      #all that need to do is concatonate the data frames after filtering any obs that are above runVolumes
      nuisance_regressors_df <- data.frame()
      for(i in 1:length(nuisance_regressors)) {
        nuisance_regressors_currun <- nuisance_regressors[[i]]
        nuisance_regressors_currun$run <- i
        runVolume = runVolumes[[i]]
        message(paste0("Current runVolumes:", runVolumes[[i]]))
        if(length(nuisance_regressors_currun[[1]]) < runVolume) {message("Warning: Nuisance Regressors have fewer observations than runVolumes")}
        nuisance_regressors_currun <- dplyr::slice(nuisance_regressors_currun, 1:runVolume) %>% as.data.frame()
        nuisance_regressors_df <- bind_rows(nuisance_regressors_df, nuisance_regressors_currun)}
      } else {
      nuisance_regressors <- NULL
    }

  if(is.null(runsToOutput)) {runsToOutput = c(1:length(unique(signals_aligned[[1]])))}
  timeOffset = rep(0, length(unique(signals_aligned[[1]])))
  dimnames(dmat)[[2L]] <- names(signals_aligned)
  if(!is.null(dropVolumes)) {dmat <- dmat[runsToOutput,,drop=FALSE] }
  #add in regressors befor convolution?
  #returns a 2-d list of runs x regressors. Needs to stay as list since runs vary in length, so aggregate is not rectangular
  #each element in the 2-d list is a 2-d matrix: trials x (onset, duration, value)

  #subfunction used by the summed runs and separate runs convolution steps
  convolve_regressor <- function(reg, vols, normalization="none", high_pass=NULL) {
    #check for the possibility that the onset + event duration exceeds the number of good volumes in the run (e.g., if truncated for high movement)
    if (any(whichHigh <- (reg[,"onset"] + reg[,"duration"]) > vols)) {
      reg <- reg[!whichHigh,]
    }

    #see hrf_convolve_normalize for implementation details
    if (normalization == "evtmax_1.0") {
      #each event is 1.0-normalized
      x <- hrf_convolve_normalize(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"], rt=tr, normeach=TRUE, center_values=center_values, parmax1=parmax1)
    } else if (normalization == "durmax_1.0") {
      #peak amplitude of hrf is 1.0 (before multiplying by parametric value) based on stimulus duration (stims > 10s approach 1.0)
      x <- hrf_convolve_normalize(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"], rt=tr, normeach=FALSE, center_values=center_values, parmax1=parmax1)
    } else {
      #no normalization
      x <- fmri.stimulus(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"], rt=tr, center_values=center_values, convolve=convolve, parmax1=parmax1)
    }

    #apply high-pass filter after convolution if requested
    if (!is.null(high_pass)) {
      x <- fir1Bandpass(x, TR=tr, low=high_pass, high=1/tr/2, plotFilter=FALSE, forward_reverse=TRUE, padx=1, detrend=1)
    }

    return(x)

  }

  #concatenate regressors across runs by adding timing from MR files.
  runtiming <- cumsum(runVolumes)*tr #timing in seconds of the start of successive runs

  #for visualization of events, return concatenated onsets (some code redundancy below)
  #note that we want to add the run timing from the r-1 run to timing for run r.
  #example: run 1 is 300 volumes with a TR of 2.0
  # thus, run 1 has timing 0s .. 298s, and the first volume of run 2 would be 300s
  concat_onsets <- lapply(1:dim(dmat)[2L], function(reg) {
    thisreg <- dmat[,reg]
    concat_reg <- do.call(c, lapply(1:length(thisreg), function(run) {
      timing <- thisreg[[run]]
      timing[,"onset"] <- timing[,"onset"] + ifelse(run > 1, runtiming[run-1], 0)
      timing[timing[,"value"] != 0, "onset"] #remove exact 0 values from "events"
    }))

    concat_reg
  })
  names(concat_onsets) <- dimnames(dmat)[[2L]]

  if (convolve_wi_run) {
    #create an HRF-convolved version of the list
    dmat.convolve <- lapply(1:dim(dmat)[1L], function(i) {
      run.convolve <- lapply(1:dim(dmat)[2L], function(j) {
        reg <- dmat[[i,j]] #regressor j for a given run i
        convolve_regressor(reg, runVolumes[i], normalizations[j], high_pass=high_pass)
      })

      df <- do.call(data.frame, run.convolve) #pull into a data.frame with ntrials rows and nregressors cols (convolved)
      names(df) <- dimnames(dmat)[[2L]]
      return(df)
    })
  } else {
    #issue with convolution of each run separately is that the normalization and mean centering are applied within-run
    #in the case of EV, for example, this will always scale the regressor in terms of relative differences in value within run, but
    #will fail to capture relative differences across run (e.g., if value tends to be higher in run 8 than run 1)

    dmat_sumruns <- lapply(1:dim(dmat)[2L], function(reg) {
      thisreg <- dmat[,reg]
      concattiming <- do.call(rbind, lapply(1:length(thisreg), function(run) {
        timing <- thisreg[[run]]
        timing[,"onset"] <- timing[,"onset"] + ifelse(run > 1, runtiming[run-1], 0)
        timing
      }))

      #convolve concatenated events with hrf
      all.convolve <- convolve_regressor(concattiming, sum(runVolumes), normalizations[reg], high_pass=high_pass)

      #now, to be consistent with code below (and elsewhere), split back into runs
      splitreg <- split(all.convolve, do.call(c, sapply(1:length(runVolumes), function(x) { rep(x, runVolumes[x]) })))

      return(splitreg)

    })

    #now have a list of length(regressors) with length(runs) elements.
    #need to reshape into a list of data.frames where each data.fram is a run with regressors
    dmat.convolve <- lapply(1:length(dmat_sumruns[[1L]]), function(run) {
      df <- data.frame(lapply(dmat_sumruns, "[[", run))
      names(df) <- dimnames(dmat)[[2L]]
      return(df)
    })

  }

  #handle the addition of temporal dervitives
  if (add_derivs) {
    message("Adding temporal derivatives of each substantive regressor orthogonalized against design matrix")

    dmat.convolve <- lapply(dmat.convolve, function(run) {
      dmat <- as.matrix(run) #need as a matrix for lm call

      dmat_derivatives <- do.call(cbind, lapply(1:ncol(dmat), function(col) {
        dx <- c(0, diff(dmat[,col]))

        ##orthogonalize wrt design
        return(residuals(lm(dx ~ dmat)))
      }))

      colnames(dmat_derivatives) <- paste0("d_", colnames(dmat))

      cbind(run, dmat_derivatives) #return design matrix with derivatives added
    })
  }




  #dmat.convolve should now be a 1-d runs list where each element is a data.frame of convolved regressors.
  names(dmat.convolve) <- paste0("run", runsToOutput)

  #Write timing files to disk for analysis by AFNI, FSL, etc.
  if (!is.null(writeTimingFiles)) {
    dir.create(output_directory, recursive=TRUE, showWarnings=FALSE)

    if ("convolved" %in% writeTimingFiles) {
      #write convolved regressors
      #AFNI amplitude modulation forces a mean and deviation from the mean regressor for each effect
      #as a result, if two parametric influences occur at a given time, it leads to perfect collinearity.
      conv_concat <- list()
      lapply(1:length(dmat.convolve), function(r) {
        lapply(1:length(dmat.convolve[[r]]), function(v) {
          regName <- names(dmat.convolve[[r]])[v]
          fname <- paste0(names(dmat.convolve)[r], "_", regName, ".1D")
          toWrite <- plyr::round_any(dmat.convolve[[r]][[v]], .000001)
          conv_concat[[regName]] <<- c(conv_concat[[regName]], toWrite) #add for concatenated 1D file
          write.table(toWrite, file=file.path(output_directory, fname), sep="\n", eol="\n", quote=FALSE, col.names=FALSE, row.names=FALSE)
        })
      })

      #write run-concatenated convolved regressors (for use in AFNI)
      lapply(1:length(conv_concat), function(v) {
        fname <- paste0(names(conv_concat)[v], "_concat.1D")
        write.table(conv_concat[[v]], file=file.path(output_directory, fname), sep="\n", eol="\n", quote=FALSE, col.names=FALSE, row.names=FALSE)
      })

    }

    if ("FSL" %in% writeTimingFiles) {
      for (i in 1:dim(dmat)[1L]) {
        for (reg in 1:dim(dmat)[2L]) {
          regout <- dmat[[i,reg]]
          if (center_values && !all(regout[,"value"] == 0.0)) {
            #remove zero-value events from the regressor
            regout <- regout[regout[,"value"] != 0, ]

            #now mean center values (unless there is no variation, such as a task indicator function)
            if (sd(regout[,"value"]) > 0) { regout[,"value"] <- regout[,"value"] - mean(regout[,"value"]) }
          }

          fname <- paste0("run", runsToOutput[i], "_", dimnames(dmat)[[2L]][reg], "_FSL3col.txt")
          write.table(regout, file=file.path(output_directory, fname), sep="\t", eol="\n", col.names=FALSE, row.names=FALSE)
        }
      }
    }

    if ("AFNI" %in% writeTimingFiles) {
      #use dmBLOCK-style regressors: time*modulation:duration. One line per run

      #need to unify multiple values that share onset time
      #time*modulation1,modulation2:duration
      regonsets <- apply(dmat, 2, function(reg) {
        unname(do.call(c, lapply(reg, function(run) { run[,"onset"]})))
      })

      regdurations <- apply(dmat, 2, function(reg) {
        unname(do.call(c, lapply(reg, function(run) { run[,"duration"]})))
      })



      #magical code from here: http://stackoverflow.com/questions/22993637/efficient-r-code-for-finding-indices-associated-with-unique-values-in-vector
      first_onset_duration <- paste(regonsets[1,], regdurations[1,]) #use combination of onset and duration to determine unique dmBLOCK regressors
      dt = as.data.table(first_onset_duration)[, list(comb=list(.I)), by=first_onset_duration] #just matches on first row (should work in general)

      lapply(dt$comb, function(comb) {
        combmat <- dmat[,comb, drop=F]
        #onsets and durations are constant, amplitudes vary
        runvec <- c() #character vector of AFNI runs (one row per run)
        for (i in 1:dim(combmat)[1L]) {
          runonsets <- combmat[[i,1]][,"onset"] #just use first regressor of combo to get vector onsets and durations (since combinations, by definition, share these)
          rundurations <- combmat[[i,1]][,"duration"]
          runvalues <- do.call(cbind, lapply(combmat[i,], function(reg) { reg[,"value"] }))

          #AFNI doesn't like us if we pass in the boxcar ourselves in the dmBLOCK format (since it creates this internally). Filter out.
          indicatorFunc <- apply(runvalues, 2, function(col) { all(col == 1.0)} )
          if (any(indicatorFunc)) { runvalues <- runvalues[,-1*which(indicatorFunc), drop=FALSE] }

          #if the indicator regressor was the only thing present, revert to the notation TIME:DURATION notation for dmBLOCK (not TIME*PARAMETER:DURATION)
          if (ncol(runvalues) == 0L) {
            runvec[i] <- paste(sapply(1:length(runonsets), function(j) {
              paste0(plyr::round_any(runonsets[j], .000001), ":", plyr::round_any(rundurations[j], .000001))
            }), collapse=" ")
          } else {
            runvec[i] <- paste(sapply(1:length(runonsets), function(j) {
              paste0(plyr::round_any(runonsets[j], .000001), "*", paste(plyr::round_any(runvalues[j,], .000001), collapse=","), ":", plyr::round_any(rundurations[j], .000001))
            }), collapse=" ")
          }
        }

        writeLines(runvec, file.path(output_directory, paste0(paste(dimnames(combmat)[[2L]], collapse="_"), "_dmBLOCK.txt")))
      })

    }
  }
  if(!is.null(nuisance_regressors)) {
    for(i in 1:length(dmat.convolve)) {
      nuisance_regressors_currun <- dplyr::filter(nuisance_regressors_df, run == i) %>% dplyr::select(-run)
      dmat.convolve[[i]] <- cbind(dmat.convolve[[i]], nuisance_regressors_currun)
    }
    #nuisance_regressors_df_split <- split(nuisance_regressors_df, nuisance_regressors_df$run)
    #dmat_changed <- lapply(dmat, function(x) {cbind(x, nuisance_regressors_df_split[[x]])})
    #dmat.convolve <- cbind(dmat.convolve, nuisance_regressors_df)
  }
  collinearityDiag.raw <- apply(dmat, 1, function(run) {
    #custom regressors are not guaranteed to have ntrials as the dimension. Remove them from raw diagnostics since they're not on the same trial grid
    #rlengths <- sapply(run, length)
    custom_reg <- grep("^custom_", names(run))
    if (length(custom_reg) > 0L) { run <- run[-1*custom_reg]}

    #check correlations among regressors for trial-wise estimates
    cmat <- do.call(data.frame, lapply(run, function(regressor) {
      regressor[,"value"]
    }))

    #remove any constant columns (e.g., task indicator regressor for clock) so that VIF computation is sensible
    zerosd <- sapply(cmat, sd)
    cmat_noconst <- cmat[,zerosd != 0.0, drop=FALSE]

    if (ncol(cmat_noconst) == 0L) {
      #if only task indicator regressors are included, then they cannot be collinear before convolution since there is no variability
      return(list(r=NA_real_, vif=NA_real_))
    } else {
      corvals <- suppressWarnings(cor(cmat, use="pairwise.complete.obs")) #drop warnings about SD of 0 since we want it to populate NAs there.
      vifMat <- data.frame(cbind(dummy=rnorm(nrow(cmat_noconst)), cmat_noconst)) #add dummy constant for vif
      vifForm <- as.formula(paste("dummy ~ 1 +", paste(names(cmat_noconst), collapse=" + ")))

      varInfl <- tryCatch(car::vif(lm(vifForm, data=vifMat)), error=function(e) { rep(NA, ncol(cmat_noconst)) }) #return NA if failure
      return(list(r=corvals, vif=varInfl))
    }
  })

  collinearityDiag.convolve <- lapply(dmat.convolve, function(run) { #apply(dmat.convolve, 1, function(run) {
    corvals <- cor(run, use="pairwise.complete.obs")
    vifMat <- data.frame(cbind(dummy=rnorm(nrow(run)), run)) #add dummy constant for vif
    vifForm <- as.formula(paste("dummy ~ 1 +", paste(names(run), collapse=" + ")))

    varInfl <- tryCatch(car::vif(lm(vifForm, data=vifMat)), error=function(e) { NA }) #return NA if failure
    list(r=corvals, vif=varInfl)
  })

  #add baseline terms to convolved design matrices
  if (baselineCoefOrder > -1L) {
    dmat.convolve <- lapply(dmat.convolve, function(r) {
      n <- names(r)
      if (baselineParameterization == "Legendre") {
        #consistent with AFNI approach, use Legendre polynomials, which are centered at zero to allow for appropriate baseline
        unnormalized.p.list <- legendre.polynomials( baselineCoefOrder, normalized=FALSE )
        #evaluate polynomial between -1 and 1, which is where its desirable centered behavior exists.
        #although the functions are centered at 0, evaluating them on a given grid may lead to slight deviations from mean=0. Thus, center perfectly.
        baseline <- polynomial.values(polynomials=unnormalized.p.list, x=seq(-1,1, length.out=nrow(r)))
        baseline[2:length(baseline)] <- lapply(baseline[2:length(baseline)], function(v) { v - mean(v) }) #don't center constant!

        names(baseline) <- paste0("base", 0:baselineCoefOrder)
        d <- cbind(r, baseline)
      } else {
        #compute polynomials that are orthogonal to design effects
        #this may be somewhat dubious. for example, a linear trend in a task regressor would be preserved because it is given preference over baseline
        d <- data.frame(fmri.design(r, order=baselineCoefOrder))
        names(d) <- c(n, paste0("base", 0:baselineCoefOrder))
      }
      d
    })

  }
  obj <- list(design=dmat, design.convolve=dmat.convolve, collin.raw=collinearityDiag.raw, collin.convolve=collinearityDiag.convolve, concat_onsets=concat_onsets, runVolumes=runVolumes)
  out <- visualizeDesignMatrix(concatDesignRuns(obj))
  if (plot == TRUE) {plot(out)}
  return(list(design=dmat, design.convolve=dmat.convolve, collin.raw=collinearityDiag.raw, collin.convolve=collinearityDiag.convolve, concat_onsets=concat_onsets, runVolumes=runVolumes, visualizeDesignMatrixOut = out))

}

#' Concatenate design matrices for each run to form a single design with unique baselines per run (ala AFNI)
#'
#' @importFrom plyr rbind.fill
#' @export
concatDesignRuns <- function(d) {

  d_allruns <- do.call(rbind.fill, lapply(1:length(d$design.convolve), function(r) {
    thisrun <- d$design.convolve[[r]]
    basecols <- grepl("base", names(thisrun))
    ##note that this will rename repeated names into something like ev and ev.1, which is good
    names(thisrun) <- gsub("base", paste0("run", r, "base"), names(thisrun))
    thisrun
  }))

  d_allruns[which(is.na(d_allruns), arr.ind=TRUE)] <- 0


  d_allruns <- as.matrix(d_allruns) #needed for lm
  d_allruns
}


#' Visualize design matrix, including event onset times and run boundaries
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_bw facet_grid geom_vline ggsave
#' @importFrom reshape2 melt
#' @export
visualizeDesignMatrix <- function(d, outfile=NULL, runboundaries=NULL, events=NULL, includeBaseline=TRUE) {

  if (!includeBaseline) {
    d <- d[,!grepl("(run[0-9]+)*base", colnames(d))]
  }

  print(round(cor(d), 3))
  d <- as.data.frame(d)
  d$volume <- 1:nrow(d)
  d.m <- melt(d, id.vars="volume")
  g <- ggplot(d.m, aes(x=volume, y=value)) + geom_line(size=1.2) + theme_bw(base_size=15) + facet_grid(variable ~ ., scales="free_y")

  colors <- c("black", "blue", "red", "orange") #just a hack for color scheme right now

  if (!is.null(runboundaries)) {
    g <- g + geom_vline(xintercept=runboundaries, color=colors[1L])
  }

  if (!is.null(events)) {
    for (i in 1:length(events)) {
      g <- g + geom_vline(xintercept=events[[i]], color=colors[i+1])
    }
  }

  if (!is.null(outfile)) {
    ggsave(filename=outfile, plot=g, width=21, height=9)
  }

  return(invisible(g))
}
