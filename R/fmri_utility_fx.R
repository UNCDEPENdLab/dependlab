#' Helper function used by the summed runs and separate runs convolution steps
#' @param reg A matrix for a regressor containing onset, duration, and value on the columns
#' @param vols The number of volumes in the convolved output (before applying \code{drop_volumes})
#' @param drop_volumes The number of volumes to drop from the beginning of the regressor
#' @param convolve If \code{TRUE}, the regressor is convolved with the HRF. If \code{FALSE},
#'          the regressor values are simply aligned onto the time grid without convolution
#'          based on the corresponding onsets, durations, and values.
#' @param normalization The HRF normalization method used: "none", "durmax_1", or "evtmax_1"
#' @param convmax_1 If \code{TRUE}, rescale convolved regressors to a maximum height of 1 to equilibrate scaling
#'          across runs and subjects.
#' @param high_pass The cutoff frequency (in Hz) used for high-pass filtering. If \code{NULL}, no filtering is applied.
#' @param tr The repetition time (in seconds) for the fMRI scan.
#'
#' @author Michael Hallquist
#' @keywords internal
convolve_regressor <- function(reg, vols, drop_volumes=0, convolve=TRUE, normalization="none",
                               center_values=TRUE, convmax_1=FALSE, high_pass=NULL, tr=NULL,
                               hrf_parameters=c(a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35)) {
  if (is.null(tr) || !is.numeric(tr)) { stop("tr must be a number (in seconds)") }

  #check for the possibility that the onset of an event falls after the number of good volumes in the run
  #if so, this should be omitted from the convolution altogether
  if (any(whichHigh <- (reg[,"onset"]/tr) >= vols)) {
    if (convolve) { message("At least one event onset falls on or after last volume of run. Omitting this from model.") } #just print on convolved execution
    print(reg[whichHigh,])
    reg <- reg[!whichHigh,]
  }

  #see hrf_convolve_normalize for implementation details
  if (convolve && normalization == "evtmax_1") {
    #each event is 1.0-normalized
    x <- hrf_convolve_normalize(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"],
                                normeach=TRUE, center_values=center_values, convmax_1=convmax_1, tr=tr, hrf_parameters=hrf_parameters)
  } else if (convolve && normalization == "durmax_1") {
    #peak amplitude of hrf is 1.0 (before multiplying by parametric value) based on stimulus duration (stims > 10s approach 1.0)
    x <- hrf_convolve_normalize(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"],
                                normeach=FALSE, center_values=center_values, convmax_1=convmax_1, tr=tr, hrf_parameters=hrf_parameters)
  } else {
    #no normalization or convolution is turned off
    x <- fmri.stimulus(scans=vols, values=reg[,"value"], times=reg[,"onset"], durations=reg[,"duration"], tr=tr, center_values=center_values,
                       convolve=convolve, convmax_1=convmax_1, a1=hrf_parameters["a1"], a2=hrf_parameters["a2"],
                       b1=hrf_parameters["b1"], b2=hrf_parameters["b2"], cc=hrf_parameters["cc"])
  }

  #apply high-pass filter after convolution if requested
  if (!is.null(high_pass)) {
    x <- fir1Bandpass(x, TR=tr, low=high_pass, high=1/tr/2, plotFilter=FALSE, forward_reverse=TRUE, padx=1, detrend=1)
  }

  #If requested, drop volumes from the regressor. This should be applied after convolution in case an event happens during the
  #truncated period, but the resulting BOLD activity has not yet resolved.
  if (drop_volumes > 0) {
    x <- x[-1*(1:drop_volumes)]
  }

  return(x)
}


#' Function to convert dmat (runs x regressor list) to a time-oriented representation.
#' This yields a list of runs where each element is data.frame of volumes x regressors
#'
#' @param dmat A runs x regressors 2-d list where each element is a matrix containing
#'          onset, duration, and value for a signal
#' @param convolve If \code{TRUE} (default), convolve the time-oriented signals with an HRF
#' @param bdm_args A list of arguments passed to build_design_matrix, as well as a few fields added
#'          during the initial argument parsing. See build_design_matrix for details. Should contain:
#'          \itemize{
#'            \item convolve_wi_run TRUE/FALSE
#'            \item run_volumes Numeric vector of run length
#'            \item normalizations Character vector of HRF normalizations for each regressor.
#'              Options are "none", "durmax_1", or "evtmax_1".
#'            \item add_derivs A logical vector (\code{TRUE/FALSE}) of regressors whose temporal derivatives should
#'              be included. Temporal derivatives are only applied if \code{convolve} is \code{TRUE}.
#'            \item convmax_1 A logical vector (\code{TRUE/FALSE}) denoting whether to rescale max height to 1 after convolution
#'            \item high_pass The cutoff frequency (in Hz) used for high-pass filtering. If \code{NULL}, no filtering is applied.
#'            \item tr The repetition time (sometimes called TR) in seconds
#'            \item hrf_parameters The parameters for the double-gamma HRF
#'          }
#'
#' @author Michael Hallquist
#' @keywords internal
place_dmat_on_time_grid <- function(dmat, convolve=TRUE, bdm_args) {

  if (bdm_args$convolve_wi_run) {
    #create an HRF-convolved version of the list
    dmat_convolved <- lapply(1:dim(dmat)[1L], function(i) {
      run.convolve <- lapply(1:dim(dmat)[2L], function(j) {
        reg <- dmat[[i,j]] #regressor j for a given run i
        convolve_regressor(reg=reg, vols=bdm_args$run_volumes[i], drop_volumes = bdm_args$drop_volumes[i], convolve=convolve,
                           normalization=bdm_args$normalizations[j], center_values=bdm_args$center_values,
                           convmax_1=bdm_args$convmax_1[j], high_pass=bdm_args$high_pass, tr=bdm_args$tr,
                           hrf_parameters = bdm_args$hrf_parameters)
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
      all.convolve <- convolve_regressor(concattiming, vols=sum(bdm_args$run_volumes), drop_volumes = bdm_args$drop_volumes[i], convolve=convolve,
                                         normalization=bdm_args$normalizations[reg], center_values=bdm_args$center_values,
                                         convmax_1=bdm_args$convmax_1[j], high_pass=bdm_args$high_pass, tr=bdm_args$tr,
                                         hrf_parameters=bdm_args$hrf_parameters)

      #now, to be consistent with code below (and elsewhere), split back into runs
      splitreg <- split(all.convolve, do.call(c, sapply(1:length(run_volumes), function(x) { rep(x, run_volumes[x]) })))

      return(splitreg)
    })

    #now have a list of length(regressors) with length(runs) elements.
    #need to reshape into a list of data.frames where each data.fram is a run with regressors
    dmat_convolved <- lapply(1:length(dmat_sumruns[[1L]]), function(run) {
      df <- data.frame(lapply(dmat_sumruns, "[[", run))
      names(df) <- dimnames(dmat)[[2L]]
      return(df)
    })
  }

  #If we are convolving regressors, also consider whether to add temporal derivatives
  #handle the addition of temporal dervitives
  if (convolve && any(bdm_args$add_derivs)) {
    which_vars <- which(dimnames(dmat)[[2]] %in% names(bdm_args$signals)[bdm_args$add_derivs])
    message("Adding temporal derivatives for ", paste(dimnames(dmat)[[2]][which_vars], collapse=", "), ", orthogonalized against design matrix")

    dmat_convolved <- lapply(dmat_convolved, function(run) {
      X <- as.matrix(run) #need as a matrix for lm call

      X_derivatives <- do.call(cbind, lapply(which_vars, function(col) {
        dx <- c(0, diff(X[,col]))

        ##orthogonalize wrt design
        return(residuals(lm(dx ~ X)))
      }))

      colnames(X_derivatives) <- paste0("d_", colnames(X)[which_vars])

      cbind(run, X_derivatives) #return design matrix with derivatives added
    })
  }

  return(dmat_convolved)
}


#' This function convolves a regressor with a normalized HRF whose peak amplitude is 1.0
#'
#' It extends \code{fmri.stimulus} by allowing for two normalization approaches (building on AFNI dmUBLOCK):
#'   1) "evtmax_1": pre-convolution HRF max=1.0 normalization of each stimulus regardless of duration: identical to dmUBLOCK(1)
#'   2) "durmax_1": pre-convolution HRF max=1.0 normalization for long events (15+ sec) -- height of HRF is modulated by duration of event: identical to dmUBLOCK(0)
#'
#' @param scans The number of volumes (scans) to be output in the convolved regressor
#' @param times A vector of times (in seconds) specifying event onsets
#' @param durations A vector of durations (in seconds) for each event
#' @param values A vector of parametric values used as regressor heights prior to convolution
#' @param tr The repetition time in seconds
#' @param normeach Whether to normalize the HRF to 1.0 for each event separately. If TRUE, equivalent to dmUBLOCK(1).
#'          If FALSE, the HRF is normed overall, equivalent to dmUBLOCK(0).
#' @param rm_zeros Whether to remove zeros from events vector prior to convolution. Generally a good idea since we typically center
#'          values prior to convolution, and retaining zeros will lead them to be non-zero after mean centering.
#' @param center_values Whether to demean values vector before convolution. Default \code{TRUE}.
#' @param convmax_1 Whether to rescale the convolved regressor to a maximum height of 1.
#' @param demean_convolved Whether to demean the regressor after convolution (default: \code{TRUE})
#' @param hrf_parameters. A named vector of parameters passed to \code{fmri.stimulus} that control the shape of the double gamma HRF.
#'          Default: \code{c(a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35)}.
#'
#' @author Michael Hallquist
#' @keywords internal
hrf_convolve_normalize <- function(scans, times, durations, values, tr=1.0, normeach=FALSE, rm_zeros=TRUE,
                                   center_values=TRUE, convmax_1=FALSE, demean_convolved=FALSE,
                                   hrf_parameters=c(a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35)) {

  if (!normeach) {
    #this is my hacky way to figure out the peak value for durmax_1 setup
    #obtain an estimate of the peak HRF height of a long event convolved with the HRF at these settings of a1, b1, etc.
    hrf_boxcar <- fmri.stimulus(scans=300/tr, values=1.0, times=100, durations=100, tr=tr, demean=FALSE,  #don't mean center for computing max height
                                a1=hrf_parameters["a1"], a2=hrf_parameters["a2"], b1=hrf_parameters["b1"], b2=hrf_parameters["b2"], cc=hrf_parameters["cc"])
    hrf_max <- max(hrf_boxcar)
  }

  #N.B. this code is duplicated below in fmri.stimulus to allow the core convolution function to mean-center parametric regressors
  #without any form of height normalization, as implemented here. may be able to reduce this code here, but because each even receives
  #a unique convolution in the code below, would need to maintain centering at least.

  #remove zeros from events vector to avoid these becoming non-zero values in the hrf convolution after grand mean centering
  #for example, for negative RPEs, if one does not occur, it will have a value of zero. But after grand mean centering below, this
  #will now be treated as an "event" by the HRF convolution since the amplitude is no longer zero.
  if (rm_zeros) {
    zeros <- which(values==0.0)
    if (length(zeros) > 0L) {
      times <- times[-1.0*zeros]
      values <- values[-1.0*zeros]
      durations <- durations[-1.0*zeros]
    }
  }

  if (length(times) == 0L) {
    warning("No non-zero events for regressor to be convolved. Returning all-zero result for fMRI GLM.")
    return(rep(0, scans))
  }

  #handle mean centering of parametric values prior to convolution
  #this is useful when one wishes to dissociate variance due to parametric modulation versus stimulus occurrence
  if (center_values && !all(values==1.0)) {
    values <- values - mean(values)
  }

  #for each event, convolve it with hrf, normalize, then sum convolved events to get full timecourse
  normedEvents <- sapply(1:length(times), function(i) {
    #obtain unit-convolved duration-modulated regressor to define HRF prior to modulation by parametric regressor
    stim_conv <- fmri.stimulus(scans=scans, values=1.0, times=times[i], durations=durations[i], tr=tr, demean=FALSE, center_values=FALSE,
                               a1=hrf_parameters["a1"], a2=hrf_parameters["a2"], b1=hrf_parameters["b1"], b2=hrf_parameters["b2"], cc=hrf_parameters["cc"])
    if (normeach) {
      if (times[i] + durations[i] > scans*tr) {
        #when the event occurs at the end of the time series and is the only event (i.e., as in evtmax_1), the HRF never reaches its peak. The further it is
        #away from the peak, the stranger the stim_conv/max(stim_conv) scaling will be -- it can lead to odd between-event scaling in the regressor.
        message("Event occurs at the tail of the run. Omitting from evtmax_1 regressor to avoid strange scaling. Please check that the end of your convolved regressors matches your expectation.")
        stim_conv <- rep(0, length(stim_conv))
      } else {
        stim_conv <- stim_conv/max(stim_conv) #rescale HRF to a max of 1.0 for each event, regardless of duration -- EQUIVALENT TO dmUBLOCK(1)
      }
    } else {
      stim_conv <- stim_conv/hrf_max #rescale HRF to a max of 1.0 for long event -- EQUIVALENT TO dmUBLOCK(0)
    }

    stim_conv <- stim_conv*values[i] #for each event, multiply by parametric regressor value
  })

  tc_conv <- apply(normedEvents, 1, sum)

  # Allow for the convolved regressor to be rescaled to have a maximum height of 1.0. This normalizes
  # the range of the regressor across runs and subjects such that the betas for the regressor are on
  # similar scales, just with a change of gain... experimenting with this currently given SCEPTIC DAUC
  # regressors which tend to be highly skewed, potentially driven by behavioral parameter scaling challenges.
  # Note that this interacts with the normeach setting such that for normeach=FALSE (durmax_1), the height
  # of 1.0 will reflect a combination of the parameter and the duration. The interpretation is somewhat
  # cleaner under normeach=TRUE (evtmax_1) since the HRF has height of 1.0 for each stimulus, and then
  # rescaling to max 1.0 after convolution with the parametric value will change the relative heights of
  #the stimuli solely according to the parameter.
  if (convmax_1) {
    tc_conv <- tc_conv/max(tc_conv)
  }

  #grand mean center convolved regressor
  if (demean_convolved) { tc_conv <- tc_conv - mean(tc_conv) }

  tc_conv
}

#' Convolve a regressor with a hemodynamic response function for fMRI analysis.
#'
#' Extended from \code{fmri} package to allow for continuous-valued regressor,
#' which is passed using the values parameter.
#'
#' The function also supports mean centering of parametric regressor prior to convolution
#' to dissociate it from stimulus occurrence (when event regressor also in model)
#'
#' @param scans The number of volumes (scans) to be output in the convolved regressor
#' @param onsets A vector of times (in seconds) specifying event onsets
#' @param durations A vector of durations (in seconds) for each event
#' @param values A vector of parametric values used as regressor heights prior to convolution
#' @param center_values Whether to demean values vector before convolution
#' @param rm_zeros Whether to remove zeros from events vector prior to convolution. Generally a good idea since we typically center
#'          values prior to convolution, and retaining zeros will lead them to be non-zero after mean centering.
#' @param convolve Whether to convolve the regressor with the HRF. If FALSE, a time series of events, durations, and heights is returned.
#' @param tr The repetition time (sometimes called TR) in seconds
#' @param times A vector of times (in seconds) specifying event onsets
#' @param demean Whether to demean the regressor after convolution
#' @param a1 The a1 parameter of the double gamma
#' @param a2 The a2 parameter of the double gamma
#' @param b1 The b1 parameter of the double gamma
#' @param b2 The b2 parameter of the double gamma
#' @param cc The cc parameter of the double gamma
#' @param convmax_1 Whether to rescale the convolved regressor to a maximum height of 1.
#'
#' @export
fmri.stimulus=function(scans=1, onsets=c(1), durations=c(1), values=c(1), center_values=FALSE, rm_zeros=TRUE, convolve=TRUE,
                       tr=3, times=NULL, demean=TRUE, a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35, convmax_1=FALSE) {

  #double gamma function
  mygamma <- function(x, a1, a2, b1, b2, c) {
    d1 <- a1 * b1
    d2 <- a2 * b2
    c1 <- ( x/d1 )^a1
    c2 <- c * ( x/d2 )^a2
    res <- c1 * exp(-(x-d1)/b1) - c2 * exp(-(x-d2)/b2)
    res
  }

  #remove zeros from events vector to avoid these becoming non-zero values in the hrf convolution after grand mean centering
  #for example, for negative RPEs, if one does not occur, it will have a value of zero. But after grand mean centering below, this
  #will now be treated as an "event" by the HRF convolution since the amplitude is no longer zero.
  if (rm_zeros) {
    zeros <- which(values==0.0)
    if (length(zeros) > 0L) {
      times <- times[-1*zeros]
      values <- values[-1*zeros]
      durations <- durations[-1*zeros]
    }
  }

  if (length(times) == 0L) {
    warning("No non-zero events for regressor to be convolved. Returning all-zero result for fMRI GLM.")
    return(rep(0, scans))
  } else if (all(times >= scans*tr)) {
    #all events fall outside of the modeled time window
    return(rep(0, scans))
  }

  #handle mean centering of parametric values prior to convolution
  #this is useful when one wishes to dissociate variance due to parametric modulation versus stimulus occurrence
  if (center_values && !all(values==1.0)) {
    values <- values - mean(values)
  }

  if (is.null(times)) {
    scale <- 1
  } else {
    #upsample time grid by a factor of 100 to get best estimate of hrf at each volume
    scale <- 100
    onsets <- times/tr*scale
    durations <- durations/tr*scale
    tr <- tr/scale
    scans <- scans*scale
  }
  numberofonsets <- length(onsets)

  if (length(durations) == 1) {
    durations <- rep(durations,numberofonsets)
  } else if (length(durations) != numberofonsets)  {
    stop("Length of durations vector does not match the number of onsets!")
  }

  if (length(values) == 1) {
    #use the same regressor height (usually 1.0) for all onsets
    values <- rep(values, numberofonsets)
  } else if (length(values) != numberofonsets) {
    stop("Length of values vector does not match the number of onsets!")
  }

  if (any(onsets >= scans)) {
    #remove any events that begin outside the modeled time window
    badonsets <- onsets >= scans
    onsets <- onsets[!badonsets]
    durations <- durations[!badonsets]
    values <- values[!badonsets]
    numberofonsets <- numberofonsets - sum(badonsets)
  }

  stimulus <- rep(0, scans)

  for (i in 1:numberofonsets) {
    for (j in onsets[i]:(onsets[i]+durations[i]-1)) {
      stimulus[j] <- values[i]
    }
  }

  #always truncate the stimulus back down to the target output length
  #if a stimulus starts after the last modeled timepoint or extends into it, stimulus
  #will become longer than expected, causing a convolution error. For now, be quiet about it,
  #but perhaps we should let the user know when he/she specifies a time vector that extends into
  #unmodeled timepoints
  stimulus <- stimulus[1:scans]

  #  zero pad stimulus vector to avoid bounding/edge effects in convolve
  stimulus <- c(rep(0,20*scale),stimulus,rep(0,20*scale))

  hrf <- convolve(stimulus,mygamma(((40*scale)+scans):1, a1, a2, b1/tr, b2/tr, cc))/scale
  hrf <- hrf[-(1:(20*scale))][1:scans]
  hrf <- hrf[unique((scale:scans)%/%scale)*scale]
  dim(hrf) <- c(scans/scale,1)

  #rescale regressor to maximum height of 1.0 for scaling similarity across instances (see hrf_convolve_normalize for details)
  #only applicable to convolve regressors
  if (convmax_1 && convolve) { hrf <- hrf/max(hrf) }

  if (!convolve) {
    #just return the box car without convolving by HRF
    stimulus <- stimulus[-(1:(20*scale))][1:scans] #remove zero padding
    stimulus <- stimulus[unique((scale:scans)%/%scale)*scale] #subset the elements of the upsampled grid back onto the observed TRs
    return(stimulus)
  } else if (demean) {
    hrf - mean(hrf)
  } else {
    hrf
  }
}

#' compute a moving average smooth over a time series (here, a vector of RTs)
#'
#' used to fit smoothed RTs (\code{clock_model} object).
#' Function is an adapted version of \code{runmean} from the \code{caTools} package.
#' @keywords internal
runmean <- function(x, k=5) {
  n <- length(x)
  k2 = k%/%2
  k1 = k - k2 - 1
  y = c(sum(x[1:k]), diff(x, k))
  y = cumsum(y)/k
  y = c(rep(0, k1), y, rep(0, k2))

  idx1 = 1:k1
  idx2 = (n - k2 + 1):n

  #use mean of available data at tails
  for (i in idx1) y[i] = mean(x[1:(i + k2)])
  for (i in idx2) y[i] = mean(x[(i - k1):n])

  return(y)
}

#' Detrend a time series up to quadratic trend. Used by fir1Bandpass prior to filtering
#'
#' @param x A time series to be detrended
#' @param order The polynomial order used for detrending. 0=demean; 1=linear; 2=quadratic
#' @export
detrendts <- function(x, order=0) {
  #order 0=demean; order 1=linear; order 2=quadratic
  lin <- 1:length(x)
  quad <- lin^2

  if (order == 0)
    residuals(lm(x~1))
  else if (order == 1)
    residuals(lm(x ~ 1 + lin))
  else if (order == 2)
    residuals(lm(x ~ 1 + lin + quad))
  else
    stop("order not supported:", order)
}

#' Apply a FIR-1 bandpass filter a signal. Can low- or high-pass filter by specifying 0 for low or >= Nyquist for high.
#'
#' @param x The time series to be filtered
#' @param TR The sampling frequency in seconds
#' @param low The lower filter cutoff in Hz. Fluctuations below this frequency will be filtered out
#' @param high The upper filter cutoff in Hz. Fluctuations above this frequency will be filtered out
#' @param n The order of the filter coefficients. Should probably leave this alone in general
#'
#' @keywords internal
#' @importFrom signal fir1 filtfilt freqz
#' @export
fir1Bandpass <- function(x, TR=2.0, low=.009, high=.08, n=250, plotFilter=FALSE, forward_reverse=TRUE, padx=0, detrend=1) {
  #check for all NA
  if (all(is.na(x))) return(x)

  #n refers to filter order. 250 does quite well with typical signals
  Fs <- 1/TR
  nyq <- Fs/2

  #enforce filter upper bound at 1.0 (nyquist)
  if (high/nyq > 1.0) { high <- nyq }

  #coefficients are specified in the normalized 0-1 range.
  fir1Coef <- fir1(n, c(low/nyq, high/nyq), type="pass")

  if (plotFilter) print(freqz(fir1Coef, Fs=Fs))

  origLen <- length(x)

  #handle detrending (almost always a good idea to demean, if not detrend, for fourier series to be valid!)
  if (!is.null(detrend) && detrend >= 0)
    x <- detrendts(x, order=detrend)

  #zero-pad data, if requested
  x <- c(x, rep(0*x, padx))

  #as the order of the filter exceeds the length of the time series,
  #some sort of phase distortion is introduced.
  #forward+reverse filtering cleans it up
  if (forward_reverse) xfilt <- filtfilt(fir1Coef, x)
  else xfilt <- filter(fir1Coef, x)

  return(xfilt[1:origLen])
}


#' Concatenate design matrices for each run to form a single design with unique baselines per run (ala AFNI)
#'
#' @param d A design matrix object created by \code{build_design_matrix}. The $design_convolved element will be used for concatenation.
#' @param convolved If \code{TRUE} (the default), concatenate the convolved design matrix. If \code{FALSE}, use the unconvolved.
#' @importFrom dplyr bind_rows
#' @export
concat_design_runs <- function(d, convolved=TRUE) {

  if (!is.list(d) || is.null(d$design_convolved)) { stop("Cannot identify $design_convolved element of object") }

  to_concat <- if (convolved) { d$design_convolved } else { d$design_unconvolved }
  d_allruns <- do.call(bind_rows, lapply(1:length(to_concat), function(r) {
    thisrun <- to_concat[[r]]
    basecols <- grepl("base", names(thisrun))
    ##note that this will rename repeated names into something like ev and ev.1, which is good
    names(thisrun) <- gsub("base", paste0("run", r, "base"), names(thisrun))
    thisrun
  }))

  d_allruns[which(is.na(d_allruns), arr.ind=TRUE)] <- 0

  d_allruns <- as.matrix(d_allruns) #needed for lm
  attr(d_allruns, "run_names") <- names(to_concat) #keep the run names around for tracking
  if (length(to_concat) > 1L) {
    run_boundaries <- c(1, cumsum(sapply(to_concat[1:length(to_concat) - 1], nrow) + 1)) #keep the run names around for tracking
    names(run_boundaries) <- names(to_concat)
  } else {
    run_boundaries <- 1 #just first volume
    names(run_boundaries) <- names(to_concat)[1]
  }

  attr(d_allruns, "run_boundaries") <- run_boundaries
  d_allruns
}


#' Visualize design matrix, including event onset times and run boundaries
#'
#' @param d a concatenated design matrix created by \code{build_design_matrix} and passed to \code{concat_design_runs}
#' @param outfile a filename used to export the design matrix using \code{ggsave}
#' @param run_boundaries a named vector of positions in the time series where a run boundary occurs (used for plotting)
#' @param events a named list of vectors containing the times of each event
#' @param include_baseline whether to display the baseline regressors in the design.
#' @importFrom ggplot2 ggplot aes geom_line theme_bw facet_grid geom_vline ggsave scale_color_brewer scale_color_discrete guides guide_legend
#' @importFrom tidyr gather
#' @export
visualize_design_matrix <- function(d, outfile=NULL, run_boundaries=NULL, events=NULL, include_baseline=FALSE) {

  #this needs to come before removal of baseline because attributes are dropped at subset
  if (is.null(run_boundaries) && !is.null(attr(d, "run_boundaries"))) {
    #check whether run_boundaries are attached to the convolved design as an attribute and use this
    run_boundaries <- attr(d, "run_boundaries")
    run_names <- attr(d, "run_names")
  } else {
    if (is.null(names(run_boundaries))) {
      run_names <- paste0("run", 1:length(run_boundaries))
    } else {
      run_names <- names(run_boundaries)
    }
  }

  if (!include_baseline) {
    d <- d[,!grepl("(run[0-9]+)*base", colnames(d))]
  } else {
    d <- d[,!grepl("(run[0-9]+)*base0", colnames(d))] #always remove constant
  }

  print(round(cor(d), 3))
  d <- as.data.frame(d)
  d$volume <- 1:nrow(d)
  d.m <- d %>% gather(key="variable", value="value", -volume)
  g <- ggplot(d.m, aes(x=volume, y=value)) + geom_line(size=1.2) + theme_bw(base_size=15) + facet_grid(variable ~ ., scales="free_y")

  if (!is.null(run_boundaries)) {
    rundf <- data.frame(run=run_names, boundary=run_boundaries)
    g <- g + geom_vline(data=rundf, aes(xintercept=boundary, color=run), size=1.3) + scale_color_discrete("Run") + #scale_color_brewer("Run", palette="Blues")
      #theme(legend.key = element_rect(size = 2), legend.key.size = unit(1.5, 'lines'))
      guides(color=guide_legend(keywidth=0.3, keyheight=1.5, default.unit="lines")) #not beautifully spaced, but come back to this later...
  }

  colors <- c("black", "blue", "red", "orange") #just a hack for color scheme right now

  if (!is.null(events)) {
    for (i in 1:length(events)) {
      eventdf <- data.frame(name=names(events)[i], positions=events[[i]])
      g <- g + geom_vline(data=eventdf, aes(xintercept=events, color=name)) + scale_color_brewer("Event", palette="Greens")
    }
  }

  if (!is.null(outfile)) {
    ggsave(filename=outfile, plot=g, width=21, height=9)
  }

  return(invisible(g))
}
