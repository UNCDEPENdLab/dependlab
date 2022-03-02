## R versions of deconvolution algorithms for testing in GIMME
## Ported from MATLAB by Michael Hallquist, September 2015

#' R port of Bush and Cisler 2013, Magnetic Resonance Imaging
#' Adapted from the original provided by Keith Bush
#'
#' @param BOLDobs observed BOLD timeseries
#' @param kernel  assumed kernel of the BOLD signal (e.g., from spm_hrf)
#' @param nev_lr  learning rate for the assignment of neural events. Default: .01
#' @param epsilon relative error change (termination condition). Default: .005
#' @param beta slope of the sigmoid transfer function (higher = more nonlinear)
#' @param normalize whether to unit-normalize (z-score) \code{BOLDobs} before deconvolution. Default: TRUE
#' @param trim_kernel whether to remove the first K time points from the deconvolved vector, corresponding to
#'            kernel leftovers from convolution. Default: TRUE
#'
#' @details
#' This function deconvolves the BOLD signal using Bush 2011 method
#'
#' Author:      Keith Bush, PhD
#' Institution: University of Arkansas at Little Rock
#' Date:        Aug. 9, 2013
#'
#' The original code did not unit normalize the BOLD signal in advance, but in my testing, this
#' proves useful in many cases (unless you want to mess with the learning rate a lot), especially
#' when the time series has a non-zero mean (e.g., mean 100).
#'
#' @return A time series of the same length containing reconstructed neural events
#' @author Keith Bush
#' @importFrom stats runif
#' @export
deconvolve_nlreg_r <- function(BOLDobs, kernel, nev_lr=.01, epsilon=.005, beta=40, normalize=TRUE, trim_kernel=TRUE) {

  if (normalize) { BOLDobs <- as.vector(scale(BOLDobs)) }

  ## Determine time series length
  N = length(BOLDobs)

  ##Calc simulation steps related to simulation time
  K = length(kernel)
  A = K - 1 + N

  ##Termination Params
  preverror = 1e9 #previous error
  currerror = 0   #current error

  ##Construct random activation vector (fluctuate slightly around zero between -2e-9 and 2e-9)
  activation = rep(2e-9, A)*runif(A) - 1e-9

  ##Presolve activations to fit target_adjust as encoding
  max_hrf_id_adjust = which.max(kernel) - 1 #element of kernel 1 before max
  BOLDobs_adjust = BOLDobs[max_hrf_id_adjust:N]
  pre_encoding = BOLDobs_adjust - min(BOLDobs_adjust)
  pre_encoding = pre_encoding/max(pre_encoding) #unit normalize
  encoding = pre_encoding
  activation[K:(K-1 + length(BOLDobs_adjust))] = (1/beta)*log(pre_encoding/(1-pre_encoding))

  while (abs(preverror-currerror) > epsilon) {

    ##Compute encoding vector
    encoding = sigmoid(activation, beta)

    ##Construct feature space
    #message("encoding is vec: ", is.vector(encoding), ", len:", length(encoding), ", K is: ", K)
    feature = dependlab:::generate_feature_armadillo(encoding, K)

    ##Generate virtual bold response by multiplying feature (N x K) by kernel (K x 1) to get N x 1 estimated response
    ytilde = feature[K:nrow(feature),] %*% kernel

    ##Convert to percent signal change
    meanCurrent = mean(ytilde)
    brf = ytilde - meanCurrent
    brf = brf/meanCurrent

    ##Compute dEdbrf
    dEdbrf = brf - BOLDobs

    ##Assume normalization does not impact deriv much.
    dEdy = dEdbrf

    ##Precompute derivative components
    dEde = diag(K) %*% kernel
    back_error = c(rep(0, K-1), dEdy, rep(0, K-1))

    ##Backpropagate Errors
    delta <- rep(0, A)

    for (i in 1:A) {
      active = activation[i]
      deda = dsigmoid(active, beta)
      dEda = dEde * deda
      this_error = back_error[i:(i-1+K)]
      delta[i] = sum(dEda * this_error)
    }

    ##Update estimate
    activation = activation - nev_lr * delta

    ##Iterate Learning
    preverror = currerror
    currerror = sum(dEdbrf^2)
  }

  if (trim_kernel) {
    ## remove the initial timepoints corresponding to HRF (so that returned signal matches in time and length)
    encoding <- encoding[K:length(encoding)]
  }

  return(encoding)
}


## Support functions
#' Sigmoid transform
#'
#' @param x value to be transformed by sigmoid
#' @param beta slope (steepness) of sigmoid transform
#' @keywords internal
sigmoid <- function(x, beta=1) {
  y <- 1/(1+exp(-beta*x))
  return(y)
}

#' Dsigmoid transform
#'
#' @param x value to be transformed
#' @param beta slope (steepness) of sigmoid transform
#' @keywords internal
dsigmoid <- function(x, beta=1) {
  y <- (1 - sigmoid(x, beta)) * sigmoid(x, beta)
  return(y)
}

#generate_feature has been shifted to a compiled C++ function
#benchmarking indicates that this is about 2.4x faster, on average

# @keywords internal
# generate_feature_orig <- function(encoding, K) {
#   fmatrix = matrix(0, length(encoding), K)
#   fmatrix[,1] = encoding
#
#   for (i in 2:K) {
#     fmatrix[,i] = c(rep(0, i-1), encoding[1:(length(encoding) - (i-1))])
#   }
#   return(fmatrix)
# }
#
# generate_feature_list <- function(encoding, K) {
#   fmatrix = list()
#   fmatrix[[1]] = encoding
#
#   for (i in 2:K) {
#     fmatrix[[i]] = c(rep(0, i-1), encoding[1:(length(encoding) - (i-1))])
#   }
#
#   fmatrix <- do.call(cbind, fmatrix)
#   return(fmatrix)
# }
#
# #use dplyr lag
# # @importFrom dplyr lag
# generate_feature_dplyr <- function(encoding, K) {
#   fmatrix <- sapply(0:(K-1), function(x) { dplyr::lag(encoding, n=x, default=0) })
#   return(fmatrix)
# }

#' This function deconvolves the BOLD signal using Bush 2011 method, augmented
#' by the resampling approach of Bush 2015.
#
#' @param BOLDobs observed BOLD signal
#' @param kernel assumed kernel of the BOLD signal
#' @param nev_lr learning rate for the assignment of neural events
#' @param epsilon relative error change (termination condition)
#' @param beta slope of the sigmoid transfer function
#' @param Nresample number of resampling steps for deconvolution
#' @param trim_kernel whether to remove the first K time points from the deconvolved vector, corresponding to
#'            kernel leftovers from convolution. Default: TRUE
#'
#' @return
#'   list containing the following fields:
#'     - NEVest  - the base neural event estimate
#'     - NEVmean - the mean neural event estimate
#'     - NEVstd - the std dev. of the neural event estimate
#'     - NEVcupp - the mean (upper limit 95% ci)
#'     - NEVclow - the mean (lower limit 95% ci)
#'     - BLDmean - the mean of the BOLD estimate
#'     - BLDstd - the std dev. of the BOLD estimate
#'     - BLDcupp - the mean BOLD (upper limit 95% ci)
#'     - BLDclow - the mean BOLD (lower limit 95% ci)
#'
#' @details
#' Author: Keith Bush
#' Institution: University of Arkansas at Little Rock
#' Date:        Aug. 12, 2013
#'
#' @author Keith Bush
#' @importFrom stats t.test
#' @export
deconvolve_nlreg_resample <- function(BOLDobs, kernel, nev_lr=.01, epsilon=.005, beta=40, Nresample=30, trim_kernel=TRUE) {

  #length of HRF
  Kobs <- length(kernel)

  #Scale observe via z-scoring
  BOLDobs_scale <- as.vector(scale(BOLDobs))

  #Deconvolve observed BOLD
  NEVdcv <- deconvolve_nlreg(matrix(BOLDobs, ncol=1), kernel, nev_lr, epsilon, beta, normalize = FALSE, trim_kernel = FALSE)

  #Reconvolve estimated true BOLD
  BOLDdcv_full <- convolve_dcv_hrf(t(NEVdcv), kernel)

  #remove kernel on front end
  BOLDdcv_low <- BOLDdcv_full[Kobs:length(BOLDdcv_full)]

  #Convert to a z-score representation
  BOLDdcv_scale = as.vector(scale(BOLDdcv_low))

  #==============================
  # Apply the resampling method
  #==============================
  NEVvariants <- matrix(0, nrow=Nresample, ncol=length(NEVdcv))
  BOLDvariants <- matrix(0, nrow=Nresample, ncol=length(BOLDobs_scale))

  NEVvariants[1,] <- NEVdcv
  BOLDvariants[1,] <- BOLDdcv_scale

  for (z in 2:Nresample) {
    #Compute residual
    residuals = BOLDobs_scale - BOLDdcv_scale

    #Randomize the residual order
    RND_residuals = sample(residuals)

    #Reapply the residual to the filtered BOLDobs
    RNDobs <- BOLDdcv_scale + RND_residuals

    #Deconvolve the variant
    NEVresidual <- deconvolve_nlreg(matrix(RNDobs, ncol=1), kernel, nev_lr, epsilon, beta, normalize = FALSE, trim_kernel = FALSE)

    #Store encoding of this variant
    NEVvariants[z,] = NEVresidual

    #Reconvolve to form the filtered BOLD of this variant
    BOLDdcv_full <- convolve_dcv_hrf(t(NEVresidual),kernel) #need transpose?
    BOLDdcv_low = BOLDdcv_full[Kobs:length(BOLDdcv_full)]
    BOLDdcv_res = as.vector(scale(BOLDdcv_low))
    BOLDvariants[z,] = BOLDdcv_res

  }

  #========================================================
  # Compute Performance for regular versus precision-guided
  #========================================================

  result <- list()

  ##Compute distribution over NEVvariants
  result[["NEVest"]] <- NEVdcv
  result[["NEVmean"]] <- apply(NEVvariants, 2, mean)
  result[["NEVstd"]] <- apply(NEVvariants, 2, sd)

  result[["NEVcupp"]] <- 0 * result[["NEVest"]]
  result[["NEVclow"]] <- 0 * result[["NEVest"]]

  for (i in 1:length(result[["NEVcupp"]])) {
    t_res <- t.test(NEVvariants[,i])
    result[["NEVclow"]][i] <- t_res$conf.int[1]
    result[["NEVcupp"]][i] <- t_res$conf.int[2]
  }

  #remove the initial timepoints from the deconvolved time series?
  if (trim_kernel) {
    result[c("NEVest", "NEVmean", "NEVstd", "NEVcupp", "NEVclow")] <-
      lapply(result[c("NEVest", "NEVmean", "NEVstd", "NEVcupp", "NEVclow")], function(x) {
        x[Kobs:length(x)] #trim off the kernel elements
      })
  }

  ##Compute distribution over BOLDvariants
  result[["BOLDmean"]] <- apply(BOLDvariants, 2, mean)
  result[["BOLDstd"]] <- apply(BOLDvariants, 2, sd)
  result[["BOLDcupp"]] <- 0 * result[["BOLDmean"]] #preallocate
  result[["BOLDclow"]] <- 0 * result[["BOLDmean"]]

  for (i in 1:length(result[["BOLDcupp"]])) {
    t_res <- t.test(BOLDvariants[,i])
    result[["BOLDclow"]][i] <- t_res$conf.int[1]
    result[["BOLDcupp"]][i] <- t_res$conf.int[2]
  }

  return(result)
}


#' This function takes in a matrix of generated neural events and
#' parameters describing the HRF function and convolves the neural
#' events and HRF function neural events
#'
#' @param NEVgen ROIs x time matrix of true neural events generated by the model
#' @param kernel The HRF kernel used for convolution
#'
#' @author Keith Bush
convolve_dcv_hrf <- function(NEVgen, kernel) {

  stopifnot(is.matrix(NEVgen))

  #Compute number of ROIS
  N <- nrow(NEVgen)
  Bn <- ncol(NEVgen)

  #Calc simulation steps related to simulation time
  Bk = length(kernel)

  #Allocate Memory to store model brfs
  BOLDgen <- matrix(0, nrow=N, ncol=Bn+Bk-1) # zeros(N,Bn+Bk-1);

  #Convert neural events to indices
  for (curr_node in 1:N) {

    #Superimpose all kernels into one time-series
    for (i in 1:length(NEVgen[curr_node,])) {
      BOLDgen[curr_node,i:(i+Bk-1)] <- NEVgen[curr_node,i]*t(kernel) + BOLDgen[curr_node,i:(i+Bk-1)]
    }

  }

  #Trim the excess Bk-1 time-points from result
  BOLDgen <- BOLDgen[,1:Bn]

  return(BOLDgen)
}



#####
## Wu code

#' R port of Wu et al. 2013, Medical Image Analysis
#' Adapted from the original provided by Daniele Marinazzo
#' @param data dataset to deconvolve
#' @param threshold (in SD units) for defining latent neural events
#' @param event_lag_max longest lag after an event that could generate a BOLD response
#' @param TR repetition time of scan (in seconds)
#' @keywords internal
wgr_deconv_canonhrf_par <- function(data, thr=1.0, event_lag_max, TR) {
  ### function [data_deconv event HRF adjust_global PARA] = wgr_deconv_canonhrf_par(data,thr,event_lag_max,TR)

  ### this function implements the method described in
  ### Wu et al,
  ### A blind deconvolution approach to recover effective connectivity brain networks from resting state fMRI data,
  ### Med Image Anal. 2013 Jan 29. pii: S1361-8415(13)00004-2. doi: 10.1016/j.media.2013.01.003

  ### input
  ### data, dimensions time points x number of voxels, normalized
  ### threshold, assuming data are normalized
  ### event_lag_max: maximum time from neural event to BOLD event in bins, not time
  ### (e.g. if we assume 10seconds, and TR=2s,  set the value to 10/2=5)
  ### TR is the TR parameter, in seconds.

  ### Some parts of the code (subfunction: Fit_Canonical_HRF, CanonicalBasisSet, get_parameters2) were modified from the hemodynamic response estimation toolbox(http://www.stat.columbia.edu/~martin/HRF_Est_Toolbox.zip).
  ###
  ### the code uses the parallel for loop ¡°parfor¡±. In case of older matlab versions, parfor can be changed to standard for.
  ###
  ### The default is using canonical hrf and two derivatives, as described in the paper.
  ### The function can be modified to use instead point processes, FIR model, etc.

  ##force data to explicit matrix form (time x variables) for proper looping
  ##this will allow data to be passed in as a 1-D vector for single variable problems
  if (!inherits(data, "matrix")) { data <- matrix(data, ncol=1) }

  N = nrow(data); nvar = ncol(data)
  even_new = wgr_trigger_onset(data,thr)

  p_m=3 #define what HRF to fit
  ## Options: p_m=1 - only canonical HRF
  ##          p_m=2 - canonical + temporal derivative
  ##          p_m=3 - canonical + time and dispersion derivative

  T = round(30/TR) ## assume HRF effect lasts 30s.
  data_deconv = matrix(0, nrow=N, ncol=nvar)
  HRF  = matrix(0, nrow=T, ncol=nvar)
  PARA = matrix(0, nrow=3, ncol=nvar)
  event = list()
  event_lag <- rep(0, nvar)
  ##warning off

  ##can parallelize over nvar
  for (i in 1:nvar) {
    out = wgr_adjust_onset(data[,i], even_new[[i]], event_lag_max, TR, p_m, T, N)
    data_deconv[,i] <- out$data_deconv
    HRF[,i] <- out$hrf
    event[[i]] <- out$events
    event_lag[i] <- out$event_lag
    PARA[,i] <- out$param
  }

  ##warning on
  return(list(data_deconv=data_deconv, event=event, HRF=HRF, event_lag=event_lag, PARA=PARA))

}

#' @importFrom stats fft cov
#' @keywords internal
wgr_adjust_onset <- function(dat,even_new,event_lag_max,TR,p_m,T,N) {
  ## global adjust.
  kk=1 #this is just a 1-based version of the loop iterator event_lag...
  hrf   = matrix(NA_real_, nrow=T, ncol=event_lag_max+1)
  param = matrix(NA_real_, nrow=p_m, ncol=event_lag_max+1)
  Cov_E = rep(NA_real_, event_lag_max+1)
  for (event_lag in 0:event_lag_max) {
    RR = even_new - event_lag; RR = RR[RR >= 0]
    design = matrix(0, nrow=N, ncol=1)
    design[RR,1] = 1 #add pseudo-events to design matrix
    fit = Fit_Canonical_HRF(dat,TR,design,T,p_m);
    hrf[,kk] <- fit$hrf
    param[,kk] <- fit$param
    Cov_E[kk] <- cov(fit$e) #covariance of residual
    kk = kk+1;
  }

  C   = min(Cov_E)
  ind = which.min(Cov_E)
  ad_global = ind - 1 #begin with 0.
  even_new = even_new - ad_global
  even_new = even_new[even_new>=0]
  hrf = hrf[,ind] #keep only best HRF (minimize error of pseudo-event timing)
  param = param[,ind] #keep only best params

  ## linear deconvolution.
  H = fft(c(hrf, rep(0, N-T)))  ##    H=fft([hrf; zeros(N-T,1)]);
  M = fft(dat)
  data_deconv = Re(fft(Conj(H)*M/(H*Conj(H)+C), inverse=TRUE)/length(H)) ## only keep real part -- there is a tiny imaginary residue in R

  return(list(data_deconv=data_deconv, hrf=hrf, events=even_new, event_lag=ad_global, param=param))
}

#' @keywords internal
wgr_trigger_onset <- function(mat, thr) {
  ##function [oneset] = wgr_trigger_onset(mat,thr)
  N = nrow(mat); nvar = ncol(mat)
  mat = apply(mat, 2, scale) #z-score columns
  oneset <- list()
  ## Computes pseudo event.
  for (i in 1:nvar) {
    oneset_temp = c()
    for (t in 2:(N-1)) {
      if (mat[t,i] > thr && mat[t-1,i] < mat[t,i] && mat[t,i] > mat[t+1,i]) { ## detects threshold
        oneset_temp = c(oneset_temp, t)
      }
    }
    oneset[[i]] = oneset_temp
  }

  return(oneset)
}




#####
## Helper functions for Wu deconvolution algorithm
## Original code from Lindquist and Wager HRF Toolbox
#' @keywords internal
CanonicalBasisSet <- function(TR) {
  len = round(30/TR) # 30 secs worth of images
  xBF <- list()
  xBF$dt = TR
  xBF$length= len
  xBF$name = 'hrf (with time and dispersion derivatives)'
  xBF = spm_get_bf(xBF)

  v1 = xBF$bf[1:len,1]
  v2 = xBF$bf[1:len,2]
  v3 = xBF$bf[1:len,3]

  ## orthogonalize
  h = v1
  dh =  v2 - (v2 %*% v1/norm(v1, "2")^2)*v1
  dh2 =  v3 - (v3 %*% v1/norm(v1, "2")^2)*v1 - (v3 %*% dh/norm(dh, "2")^2)*dh

  ## normalize amplitude
  h = h/max(h)
  dh = dh/max(dh)
  dh2 = dh2/max(dh2)

  return(list(h=h, dh=dh, dh2=dh2))
}

#' @keywords internal
#' @importFrom MASS ginv
#' @importFrom stats convolve
Fit_Canonical_HRF <- function(tc, TR, Run, T, p) {
  ##function [hrf, e, param] = Fit_Canonical_HRF(tc,TR,Run,T,p)
  ##
  ## Fits GLM using canonical hrf (with option of using time and dispersion derivatives)';
  ##
  ## INPUTS:
  ##
  ## tc    - time course
  ## TR    - time resolution
  ## Runs  - expermental design
  ## T     - length of estimated HRF
  ## p     - Model type
  ##
  ## Options: p=1 - only canonical HRF
  ##          p=2 - canonical + temporal derivative
  ##          p=3 - canonical + time and dispersion derivative
  ##
  ## OUTPUTS:
  ##
  ## hrf   - estimated hemodynamic response function
  ## fit   - estimated time course
  ## e     - residual time course
  ## param - estimated amplitude, height and width

  len = length(Run)

  X = matrix(0, nrow=len, ncol=p)

  bf = CanonicalBasisSet(TR)
  h  = bf$h; dh = bf$dh; dh2 = bf$dh2

  v = convolve(Run,rev(h), type="open") #this is the R equivalent of conv(Run, h)
  X[,1] = v[1:len]

  if (p > 1) {
    v = convolve(Run,rev(dh), type="open")
    X[,2] = v[1:len]
  }

  if (p > 2) {
    v = convolve(Run,rev(dh2), type="open")
    X[,3] = v[1:len]
  }

  X   = cbind(rep(1, len), X) #add intercept
  b   = MASS::ginv(X) %*% tc
  e   = tc - X %*% b
  fit = X %*% b

  b = b[2:length(b)]

  if (p == 2) {
    bc = sign(b[1])*sqrt(b[1]^2 + b[2]^2)
    H = cbind(h, dh)
  } else if (p == 1) {
    bc = b[1]
    H = matrix(h, ncol=1)
  } else if (p>2) {
    bc = sign(b[1])*sqrt(b[1]^2 + b[2]^2 + b[3]^2)
    H = cbind(h, dh, dh2)
  }

  hrf = H %*% b

  param = get_parameters2(hrf,T)

  return(list(hrf=hrf, e=e, param=param))
}


get_parameters2 <- function(hdrf, t) {

  ##function [param] = get_parameters2(hdrf,t)

  ## Find model parameters
  ##
  ## Height - h
  ## Time to peak - p (in time units of TR seconds)
  ## Width (at half peak) - w

  ## Calculate Heights and Time to peak:

  ## n = t(end)*0.6;
  n = round(t*0.8)

  p = which.max(abs(hdrf[1:n]))
  h = hdrf[p]

  ##if (p > t(end)*0.6), warning('Late time to peak'), end;

  if (h > 0) {
    v = as.numeric(hdrf >= h/2)
  } else {
    v = as.numeric(hdrf <= h/2)
  }

  b = which.min(diff(v))
  v[(b+1):length(v)] = 0
  w = sum(v)

  cnt = p-1
  g = hdrf[2:length(hdrf)] - hdrf[1:(length(hdrf)-1)]

  while(cnt > 0 && abs(g[cnt]) < 0.001) {
    h = hdrf[cnt]
    p = cnt
    cnt = cnt-1
  }

  param = rep(0,3)
  param[1] = h
  param[2] = p
  param[3] = w

  return(param)
}
