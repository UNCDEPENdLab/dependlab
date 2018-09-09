#' Creates an fmri design matrix, including timing files for for AFNI or FSL.
#'
#' @param events a data.frame that includes a column for the event type (e.g. outcome vs. cue),
#'           run number (1:n), trial number (nested within run, 1:x), onset, duration of event
#' @param signals expects a list of list. The first level of lists reflects each signal (e.g. pe vs values).
#'           In the second level, BDM expects values and event (e.g. cue vs outcome). Values is a \code{data.frame}
#'           with run number (1:n), trial (1:x), and signal.
#' @param tr The repetition time of your fMRI sequence in seconds. You must specify this.
#'           This is important to specify correctly to get temporal filtering correct.
#' @param center_values A logical (\code{TRUE/FALSE}) indicating whether to center parameteric regressors prior to
#'           convolution. This is usually a good idea to remove collinearity between parametric and task indicator
#'           regressors. The default is \code{TRUE}.
#' @param hrf_parameters A named vector of HRF parameters passed to \code{fmri.stimulus} internally.
#'           The parameters are a1, a2, b1, b2, cc. Equation is (x/d1)^a1 * exp(-(x - d1)/b1) - c * (x/d2)^a2 * exp(-(x - d2)/b2).
#'           Defaults and descriptions are:
#'             a1 = 6. Controls the time delay to the peak of the positive response
#'             a2 = 12. Controls the time delay to the (negative) peak of the undershoot
#'             b1 = 0.9. Controls the dispersion (breadth) of the positive response
#'             b2 = 0.9. Controls the dispersion (breadth) of the undershoot
#'             cc = 0.35. Controls the relative scaling of the positive response versus the undershoot.
#'
#'           Note: These defaults come from Glover 1999.
#'
#'           Note. FSL double gamma has a1 = 6, a2 = 16, cc = 1/6. Not yet sure about b1 and b2.
#' @param baseline_coef_order Default -1 (no baseline). If >= 0, then design will include polynomial trends
#'    within each run (e.g. baseline_coef_order = 1 includes both an intercept and a linear trend as regressors)
#' @param baseline_parameterization Defaults to "Legendre". This adds Legendre polynomials up to \code{baseline_coef_order} (e.g., 2).
#'           The alternative is "orthogonal_polynomials", which uses \code{fmri.design} from the \code{fmri} package
#'           to add polynomial regressors that are orthgonal to substantive design factors.
#' @param run_volumes Expects a numeric vector containing the number of volumes per run. If just a single number is passed,
#'           the function assumes all runs have this number of volumes. This parameter sets
#'           the corresponding lengths of convolved regressors so that they match the MR data. Alternatively,
#'           you can pass a character vector of relevant NIfTI filenames, one per run, and build_design_matrix will
#'           calculate the number of volumes based on the 4th dimension (time) of the fMRI data. Finally, if you
#'           do not pass in this argument, build_design_matrix will take a guess that the run should end 12 seconds
#'           (or whatever you specifiy for \code{iti_post}) after the last event ends:
#'           max(onset + duration + iti_post)/tr within each run.
#' @param drop_volumes By default, all volumes are retained. If specified, this can be a vector of the number of volumes
#'           that will be removed from the \emph{beginning} of each convolved regressor. If you pass a single number (e.g., 3),
#'           this number of volumes will be dropped from each run. This is useful if you have dropped the first n volumes
#'           of your MR data, for example to handle problems with steady state magnetization.
#' @param runs_to_output A numeric vector of runs to be output. By default, all runs are preserved.
#'           This is used to model only a subset such as \code{c(1, 2, 6)}.
#' @param plot By default (\code{TRUE}), \code{build_design_matrix} will plot the design matrix in the plot window of your R session.
#'           If \code{FALSE}, the plot is not displayed, but the ggplot object is still provided in the $design_plot field.
#' @param write_timing_files When NULL (the default), the function does not write timing files to disk.
#'           This argument accepts a character vector that specifies whether to write "AFNI", "FSL",
#'           or "convolved" timing files to disk (in \code{output_directory}). AFNI files follow the
#'           dmBLOCK convention of TIME*PARAMETER:DURATION for each event. FSL files follow the three-column
#'           format of onset, duration, value. And convolved files represent a given signal convolved with the
#'           HRF such that the 1-column output is in volumes (i.e., one row per volume).
#' @param output_directory Where to output the timing files. By default, the function will output the timing files
#'           to a folder called "run_timing" in the current working directory. If such a folder does not exist,
#'           it will make a folder in your R session's current working directory.
#' @param convolve_wi_run If \code{TRUE} (the default), convolution -- and any corresponding mean centering and
#'           normalization of the heights of parametric signals -- is applied to each run separately.
#'          If FALSE, the events across runs are concatenated before convolution is applied
#'          (i.e., treating it as one long time series).
#' @param high_pass By default, \code{NULL}. If desired, pass in a number in Hz that specifies the high pass filter cutoff.
#'                    In this case a FIR-based high-pass filter will be applied to remove any low-frequency fluctuations
#'                    in the design signals after convolution. This can be useful and necessary if the MRI have been filtered,
#'                    but the regressors have not. It is important that the frequency content of both MR and design signals matches.
#'                    Some programs, including FEAT, ensure that equivalent filtering is applied to bot the Y and X sides of this
#'                    equation, but you should be clear whether your program does so, too. If it doesn't, probably best to use this
#'                    argument to filter things yourself. For example, 3dDeconvolve only handles filtering via a set of drift (polort)
#'                    regressors. If you have used another tools such as fslmaths -bptf to filter the data, the polort will not necessarily
#'                    result in the same removal of drift from regressors as was applied to the MR data.
#' @param iti_post By default, 12. Assumes 12 volumes after each run. Only necessary to specify if not supplying run_volumes and
#'                    expecting function to use events information to calculate run_volumes. Wouldn't recommend this, just a default here.
#' @param nuisance_regressors By default, \code{NULL}. If nuisance regressors specified, either expects list of
#'                    character strings for different .txt files for the nuisance regressors OR it expects a
#'                    list of data.frames (1 df per run). These values are tacked onto design_convolved
#'                    (and not convolved with HRF), so each regressor should be length of the number of
#'                    run_volumes within that run. If you pass in a list of .txt files containing nuisance regressors,
#'                    these will be read into R, truncated to run_volumes, and column-wise concatenated with
#'                    substative regressors.

#' @details
#'
#' The function outputs a list containing key aspects of the fMRI desing, including
#' the unconvolved and convolved regressors, collinearity diagnostics, the number of volumes
#' modeled in each run, and a plot of the design.
#'
#' The basic logic of the inputs to build_design_matrix is that task-related fMRI designs are organized around a set of events that occur in time and
#' have a specific duration. Furthermore, for a given event, it could be a 0/1 non-occurrence versus occurrence representation, \emph{or} the event could
#' be associated with a specific parametric value such as working memory load, reward prediction error, or expected value. These parametric effects
#' are aligned in time with an event, but there may be multiple predictions for a given event. For example, we may align a 0/1 regressor and a
#' reward prediction error the outcome phase of a task.
#'
#' Thus, the function abstracts timing-related information into \code{events} and signals, whether parametric or binary, into the \code{signals}.
#'
#' The \code{events} argument expects a \code{data.frame} that has, minimally, the following structure:
#'
#' \preformatted{
#'  > print(events)
#'      event run trial onset duration
#'        cue   1     1     4        2
#'        cue   1     2     7        2
#'    outcome   1     1     6      0.5
#'    outcome   1     2   9.5      0.5
#'        cue   2     1   1.2        2
#'        cue   2     2    12        2
#'    outcome   2     1     6      0.5
#'    outcome   2     2   9.5      0.5
#' }
#'
#' Note that you can tack on other columns to \code{events} if it useful to you. Furthermore, if you want to test different
#' durations (e.g., RT-convolved versus fixed duration versus instantaneous), you can add these as additional columns
#' (e.g., \code{duration_1s}, \code{duration_instant}, etc.). To make use of these in the design, specify the column name
#' in events in the \code{$duration} element of a given signal in the \code{signals} list. If you do not specify the
#' \code{$duration} element in \code{signals}, \code{build_design_matrix} will assume that the relevant duration is stored
#' in the \code{$duration} column of \code{events}.
#'
#' The \code{signals} argument expects a list where each element is a given signal that should be aligned with an event and that
#' has some height (e.g., 0/1 or a parametric value) prior to convolution. The signals list should be named by signal and each element should
#' be a list itself, such as the following:
#'
#' \preformatted{
#'   signals <- list(
#'     cue=list(event="cue", duration=0, value=1, normalization="none")
#'   )
#' }
#'
#' The \code{event} element specifies the mapping between a given signal and the corresponding timing in the \code{events} \code{data.frame}.
#' In essence, this is used to merge the event and signal data together. Here, we specify that the cue signal is aligned in time with the cue event.
#'
#' The \code{duration} element can be:
#' \enumerate{
#'   \item A single number, in which case this fixed duration is used for all events
#'   \item A name of the column to be used in the \code{events} \code{data.frame} (e.g., "duration_rtshift")
#'   \item Omitted altogether, in which case \code{build_design_matrix} will default to the "duration" column of \code{events}.
#' }
#'
#' The \code{value} element can be a single number (e.g., 1) in which case this height is used for all corresponding occurrences of a given event.
#' Most commonly, a fixed value is useful for modeling a 'taskness' regressor, which captures a 0/1 representation of whether an event is occurring
#' at a given moment in time. In conventional task-reated fMRI, this task indicator representation is then convolved with the HRF to model expected BOLD
#' activity due to the occurrence of an event. Alternatively, \code{value} can be a data.frame containing \code{$run}, \code{$trial}, and \code{$value}
#' columns that specify the height of the regressor at each trial. This specification is more useful for a parametric regressor, as in model-based fMRI.
#' Here is an example:
#'
#' \preformatted{
#'   signals <- list(
#'     pe=list(event="outcome", normalization="none", convmax_1=TRUE,
#'     value=data.frame(
#'       run=rep(1,5),
#'       trial=1:5,
#'       value=c(0, 10.2, -11.1, 6, 2.4, 1.5)
#'     )
#'   )
#' }
#'
#' Here, the parametrically varying prediction error signal will be aligned at the "outcome" event, have a duration copied from the
#' \code{$duration} column of \code{events}, and will have parametrically varying heights (e.g., 10.2 at trial 2) prior to convolution.
#' Note that the value \code{data.frame} need not have an entry for every trial in the run. For example, if a given signal is only relevant
#' or only occurs for some "outcome" events, the trial column might be something like \code{c(2, 6, 10)}, indicating that the parametric
#' modulator is only modeled at those trials. This is achieved by joining \code{events} with the relevant signal using \code{trial} as a key.
#'
#' The \code{$normalization} element handles the normalization of the HRF for each regressor. This can be:
#' \enumerate{
#'   \item \code{durmax_1}: pre-convolution, normalize the HRF max to 1.0 for long events (15+ sec) such that
#'             height of HRF is modulated by duration of event but maxes at 1. This is identical to dmUBLOCK(0).
#'   \item \code{evtmax_1}: pre-convolution, normalize the HRF max to 1.0 for each stimulus
#'             regardless of duration. This is identical to dmUBLOCK(1).
#'   \item \code{none}: No normalization of the HRF is performed prior to convolution.
#' }
#'
#' The optional \code{$convmax_1} element handles rescaling the \emph{convolved} regressor to a maximum height of 1.0.
#'   If TRUE for a given signal, the convolved regressor will be divided by its max, leading to a max of 1.0 across
#'   both runs (assuming \code{convolve_wi_run} is \code{TRUE}) and subjects. This may be useful for scaling the regression
#'   coefficients in voxelwise regression across subjects. For example, if the parametric signal captures similar dynamics
#'   within subjects over the experiment, but the scaling varies substantially between subjects, \code{convmax_1} can
#'   help to place the betas on an equivalent scale across subjects (assuming the MR data are also scaled similarly
#'   between subjects).
#'
#' Finally, the optional \code{add_deriv} element determines whether the temporal derivative of a regressor is added to
#'   the design matrix after convolution. Following FSL, the derivatives are computed by a first-order difference and are
#'   then residualized for other regressors in the matrix. That is, the derivatives are orthogonalized with respect to
#'   substantive regressors. By default, derivatives are not added, but if \code{TRUE} for a given signal, this will be added
#'   to the convolved design matrix.
#'
#' This function was adapted from the fitclock package (https://github.com/PennStateDEPENdLab/fitclock.git) to
#'   allow for more general creation of design matrices for fMRI analyses.
#'
#' @return A list of containing different aspects of the design matrix:
#' \itemize{
#'        \item \code{$design}: A runs x signals list containing events before convolution.
#'          Each element is a 2-D matrix containing, minimally, "trial", onset", "duration", and "value" columns.
#'          Onsets and durations are specified in seconds, consistent with FSL's 3-column format.
#'          Within each matrix, the onset, duration and value of the signal is specified.
#'        \item \code{$design_convolved}: The convolved design matrices for each run. Each element in the list contains
#'          a run. Within each design matrix, each column contains a regressor, encompassing substantive regressors,
#'          nuisance signals, and polynomial baseline regressors, if specified. Each row reflects the predicted value for each volume.
#'        \item \code{$design_unconvolved}: The unconvolved design matrices for each run. Same structure as \code{$design_convolved},
#'          but prior to convolution with the HRF.
#'        \item \code{$collin_raw}: A list containing information about the collinearity of regressors before convolution.
#'          At the highest level of the list, each element contains a run. At the second level of the list,
#'          the first element contains the correlation matrix of the regressors and the second element provides
#'          the variance inflation factor (VIF) associated with each regressor. Example: \code{design$collin_raw$run1$vif}
#'        \item \code{$collin_convolve}: A list containing information about collinearity of the convolved regressors,
#'          including substantive signals, nuisance regressors, and polynomial regressors. Follows the same structure as \code{$collin_raw}.
#'        \item \code{$concat_onsets}: A list containing concatenated event onset times for each signal. Each signal is an element of the list containing
#'          a vector of all onset times across runs. That is, the total time of run1 is added to onsets of run2 to support a combined analysis of all
#'          runs, which is common in AFNI (e.g., using -concat or multiple files to -input).
#'        \item \code{$run_volumes}: A vector containing the total number of volumes modeled for each run.
#'        \item \code{$design_plot}: A ggplot object showing the design matrix. This is generated by \code{visualize_design_matrix}.
#' }
#' @importFrom data.table as.data.table
#' @importFrom plyr round_any
#' @importFrom dplyr filter select slice bind_rows arrange "%>%"
#' @importFrom orthopolynom legendre.polynomials polynomial.values
#' @importFrom oro.nifti readNIfTI
#' @importFrom ggplot2 ggplot
#' @importFrom fmri fmri.design
#' @importFrom car vif
#' @importFrom stats as.formula cor lm residuals rnorm
#' @importFrom utils read.table write.table
#'
#' @author Michael Hallquist
#' @author Alison Schreiber
#' @examples
#'
#'   data(example_events)
#'   data(example_signals)
#'
#'   #basic convolved design matrix
#'   d <- build_design_matrix(events = example_events, signals = example_signals, tr=1.0)
#'
#'   data(example_nuisrun1) #load demo nuisance signals
#'   data(example_nuisrun2)
#'
#'   #design matrix with 0,1,2 polynomial baseline regressors and a set of nuisance regressors
#'   #this does not contain a 'taskness' regressor for cue or outcome
#'   dnuis <- build_design_matrix(events = example_events, signals = example_signals, tr=1.0,
#'     nuisance_regressors = list(example_nuisrun1, example_nuisrun2), baseline_coef_order = 2)
#'
#'   #tweak the design to add temporal derivatives for both ev and pe
#'   example_signals$pe$add_deriv <- TRUE
#'   example_signals$ev$add_deriv <- TRUE
#'
#'   #also use the evtmax_1 normalization method for both parametric signals
#'   example_signals$pe$normalization <- "evtmax_1"
#'   example_signals$ev$normalization <- "evtmax_1"
#'
#'   #finally, add a taskness regressor for cue and outcome
#'   example_signals[["cue_evt"]] <- list(value=1, event="cue", normalization="none")
#'   example_signals[["outcome_evt"]] <- list(value=1, event="outcome", normalization="none")
#'
#'   #include up to quadratic drift terms in the design, drop 3 volumes from the beginning,
#'   #and write timing files in AFNI, FSL, and convolved formats (to the "run_timing" directory).
#'   d_modified <- build_design_matrix(events = example_events, signals = example_signals, tr=1.0,
#'     baseline_coef_order=2, drop_volumes=3, write_timing_files = c("convolved", "AFNI", "FSL"))
#'
#'   #show unconvolved design
#'   plot(visualize_design_matrix(concat_design_runs(d_modified, convolved=FALSE)))
#'
#' @export
build_design_matrix=function(
  events = NULL,
  signals = NULL,
  tr=NULL, #TR of scan in seconds
  center_values=TRUE, #whether to center parametric regressors prior to convolution
  hrf_parameters=c(a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35),
  baseline_coef_order=-1L, #don't include baseline by default
  baseline_parameterization="Legendre",
  run_volumes=NULL, #vector of total fMRI volumes for each run (used for convolved regressors)
  drop_volumes=0L, #vector of how many volumes to drop from the beginning of a given run
  runs_to_output=NULL,
  plot=TRUE,
  write_timing_files=NULL,
  output_directory="run_timing",
  convolve_wi_run=TRUE, #whether to mean center parametric regressors within runs before convolution
  high_pass=NULL, #whether to apply a high-pass filter to the design matrix (e.g., to match fmri preprocessing)
  iti_post = 12,
  nuisance_regressors = NULL #allow for nuisance regression text file to be implemented. need separate file for each
) {

  #take a snapshot of arguments to build_design_matrix that we pass to subsidiary functions
  bdm_args <- as.list(environment(), all.names = TRUE)
  #validate events data.frame

  if (is.null(events)) { "You must pass in an events data.frame. See ?build_design_matrix for details." }
  if (is.null(signals)) { "You must pass in a signals list. See ?build_design_matrix for details." }
  if (is.null(tr)) { "You must pass in the tr (repetition time) in seconds. See ?build_design_matrix for details." }

  stopifnot(inherits(events, "data.frame"))
  if (!"event" %in% names(events)) { stop("events data.frame must contain event column with the name of the event") }
  if (!"trial" %in% names(events)) { stop("events data.frame must contain trial column with the trial number for each event") }
  if (!"onset" %in% names(events)) { stop("events data.frame must contain onset column with the onset time in seconds") }
  if (!"duration" %in% names(events)) { stop("events data.frame must contain duration column with the event duration in seconds") }
  if (!"run" %in% names(events)) {
    message("No run column found in events. Assuming 1 run and adding this column")
    events$run <- 1
  }

  #merge the trial-indexed signals with the time-indexed events
  #basically: put the events onto the time grid of the run based on the "event" element of the list
  signals_aligned <- lapply(signals, function(s) {
    if (is.null(s$event)) { stop("Signal does not have event element") }
    df_events <- dplyr::filter(events, event==s$event)
    df_signal <- s$value #the signal data.frame for this signal
    if (length(df_signal)==1L && is.numeric(df_signal)) { #task indicator-type regressor
      s_aligned <- df_events
      s_aligned$value <- df_signal #replicate requested height for all occurrences
    } else {
      s_aligned <- df_signal %>% dplyr::left_join(df_events, by=c("run", "trial")) %>% arrange(run, trial) #enforce match on signal side
    }

    if (length(s$duration) > 1L) { stop("Don't know how to interpret multi-element duration argument for signal: ", paste0(s$duration, collapse=", ")) }
    if(!is.null(s$duration)) {
      if (is.numeric(s$duration)) {
        s_aligned$duration <- s$duration #replicate the scalar on all rows
      } else {
        s_aligned$duration <- s_aligned[[s$duration]]
      }
    }

    #transform to make dmat happy (runs x regressors 2-d list)
    #dplyr::select will tolerate quoted names, which avoids R CMD CHECK complaints
    retdf <- s_aligned %>% dplyr::select("trial", "onset", "duration", "value")
    retsplit <- split(retdf, s_aligned$run)
    names(retsplit) <- paste0("run", names(retsplit))
    return(retsplit)
  })

  #extract the normalization for each regressor into a vector
  bdm_args$normalizations <- sapply(signals, function(s) {
    ifelse(is.null(s$normalization), "none", s$normalization) #default to none
  })

  #extract the convmax_1 settings for each regressor into a vector
  bdm_args$convmax_1 <- sapply(signals, function(s) {
    if (is.null(s$convmax_1)) {
      FALSE
    } else {
      if (!s$convmax_1 %in% c(TRUE, FALSE)) { #NB. R kindly type casts "TRUE" and 1/0 to logical for this comparison
        stop("Don't know how to interpret convmax_1 setting of: ", s$convmax_1)
      } else {
        as.logical(s$convmax_1)
      }
    }
  })

  #determine whether to add a temporal derivative for each signal
  bdm_args$add_derivs <- sapply(signals, function(s) {
    if (is.null(s$add_deriv)) {
      FALSE
    } else {
      if (!s$add_deriv %in% c(TRUE, FALSE)) { #NB. R kindly type casts "TRUE" and 1/0 to logical for this comparison
        stop("Don't know how to interpret add_deriv setting of: ", s$add_deriv)
      } else {
        as.logical(s$add_deriv)
      }
    }
  })

  #define number of runs based off of the length of unique runs in the events data.frame
  nruns <- length(unique(events$run))
  if (is.null(run_volumes)) {
    #determine the last fMRI volume to be analyzed
    run_volumes <- rep(0, nruns)

    for(i in 1:nruns) {
      for (j in 1:length(signals_aligned)) {
        currentdf <- signals_aligned[[j]][[i]] #jth signal, ith run (signals_aligned has signals at top level)

        #estimate the last moment
        highesttime <- ceiling(max(currentdf$onset + currentdf$duration + iti_post, na.rm=TRUE)/tr)
        if (highesttime > run_volumes[i]) { run_volumes[i] <- highesttime } #update run_volumes for ith run only if this signal has later event
      }
    }

    message("Assuming that last fMRI volume was 12 seconds after the onset of the last event.")
    message(paste0("Resulting volumes: ", paste(run_volumes, collapse=", ")))

  } else if (is.character(run_volumes)) {
    run_volumes_list <- c()
    for( i in 1:length(run_volumes)) {
      currentNifti <- run_volumes[i]
      out <- readNIfTI(currentNifti, read_data = FALSE)
      rv <- dim(out@.Data)[4] # grabs the 4th dimension of the matrix, which is the number of Volumes
      run_volumes_list <- c(run_volumes_list, rv)
    }
    run_volumes <- run_volumes_list
  } else if (is.numeric(run_volumes)) {
    #replicate volumes for each run if a scalar is passed
    if (length(run_volumes) == 1L) { run_volumes <- rep(run_volumes, nruns) }
    stopifnot(length(run_volumes) == nruns)
  } else {
    stop("Don't know how to handle run_volumes: ", run_volumes)
  }

  #determine which run fits should be output for fmri analysis
  if (is.null(runs_to_output)) {
    message("Assuming that all runs should be fit and run numbers are sequential ascending")
    runs_to_output <- 1:length(run_volumes) #output each run
  }

  if (length(drop_volumes) < length(run_volumes)) {
    if (drop_volumes[1L] > 0) { message("Using first element of drop_volumes for all runs: ", drop_volumes[1L]) }
    drop_volumes <- rep(drop_volumes[1L], length(run_volumes))
  }

  #handle nuisance regressors
  if (is.character(nuisance_regressors)) {
    nuisance_regressors_df <- data.frame()

    for (i in 1:length(nuisance_regressors)) {
      nuisance_regressor_currun <- read.table(nuisance_regressors[i])
      nuisance_regressors_currun$run <- i
      rv = run_volumes[i]
      print(rv)
      nuisance_regressors_currun <- dplyr::slice(nuisance_regressor_currun, (drop_volumes[i]+1):rv) %>% as.data.frame()
      nuisance_regressors_df <- bind_rows(nuisance_regressors_df, nuisance_regressor_currun)
    }
  } else if (is.list(nuisance_regressors)) {
    #assuming that a list of data.frames for each run of the data
    #all that need to do is concatonate the data frames after filtering any obs that are above run_volumes
    nuisance_regressors_df <- data.frame()
    for(i in 1:length(nuisance_regressors)) {
      nuisance_regressors_currun <- nuisance_regressors[[i]]
      stopifnot(is.data.frame(nuisance_regressors_currun))
      nuisance_regressors_currun$run <- i
      rv = run_volumes[i]
      message(paste0("Current run_volumes:", rv))
      if(nrow(nuisance_regressors_currun) < rv) { stop("Nuisance regressors have fewer observations than run_volumes") }
      nuisance_regressors_currun <- dplyr::slice(nuisance_regressors_currun, (drop_volumes[i]+1):rv) %>% as.data.frame()
      nuisance_regressors_df <- bind_rows(nuisance_regressors_df, nuisance_regressors_currun)
    }
  } else {
    nuisance_regressors <- NULL
  }

  #Build the runs x signals 2-D list
  #Note that we enforce dropped volumes (from beginning of run) below
  #This is because fmri.stimulus gives odd behaviors (e.g. constant 1s) if an onset time is negative
  dmat <- do.call(cbind, lapply(1:length(signals_aligned), function(signal) {
    lapply(signals_aligned[[signal]], function(run) { as.matrix(run) })
  }))

  #only retain runs to be analyzed
  dmat <- dmat[runs_to_output,,drop=FALSE]
  run_volumes <- run_volumes[runs_to_output] #need to subset this, too, for calculations below to match
  drop_volumes <- drop_volumes[runs_to_output] #need to subset this, too, for calculations below to match

  #run_volumes and drop_volumes are used by convolve_regressor to figure out the appropriate regressor length
  bdm_args$run_volumes <- run_volumes #copy into argument list
  bdm_args$drop_volumes <- drop_volumes #copy into argument list

  #make sure the columns of the 2-D list are named by signal
  dimnames(dmat)[[2L]] <- names(signals_aligned)

  #convert the trial-oriented dmat to a time-oriented dmat_convolved.
  #also get an unconvolved version on the time grid for diagnostics.
  dmat_convolved <- place_dmat_on_time_grid(dmat, convolve=TRUE, bdm_args)
  dmat_unconvolved <- place_dmat_on_time_grid(dmat, convolve=FALSE, bdm_args)

  #dmat_convolved should now be a 1-d runs list where each element is a data.frame of convolved regressors.
  names(dmat_convolved) <- names(dmat_unconvolved) <- paste0("run", runs_to_output)

  time_offset <- tr*drop_volumes #how much time is being dropped from the beginning of the run (used to change event onsets)
  run_volumes <- run_volumes - drop_volumes #update run_volumes to reflect drops: elementwise subtraction of dropped volumes from full lengths

  if (time_offset[1L] > 0) {
    if (length(unique(time_offset)) == 1L) { to_print <- time_offset[1L]
    } else { to_print <- paste(time_offset, collapse=", ") }
    message("Based on drop_volumes and tr, subtracting ", to_print, "s from event onsets")
  }

  #Change the timing of the regressors in dmat according to drop_volumes
  for (i in 1:dim(dmat)[1]) {
    for (j in 1:dim(dmat)[2]) {
      df <- dmat[[i,j]]
      df[,"onset"] <- df[,"onset"] - time_offset[i]
      if (min(df[,"onset"] < 0)) {
        message("For run ", dimnames(dmat)[[1]][i], ", regressor ", dimnames(dmat)[[2]][j], ", there are ",
                sum(df[,"onset"] < 0), "events before time 0")
      }
      dmat[[i,j]] <- df
    }
  }

  #Write timing files to disk for analysis by AFNI, FSL, etc.
  if (!is.null(write_timing_files)) {
    dir.create(output_directory, recursive=TRUE, showWarnings=FALSE)

    if ("convolved" %in% write_timing_files) {
      #write convolved regressors
      #AFNI amplitude modulation forces a mean and deviation from the mean regressor for each effect
      #as a result, if two parametric influences occur at a given time, it leads to perfect collinearity.
      conv_concat <- list()
      lapply(1:length(dmat_convolved), function(r) {
        lapply(1:length(dmat_convolved[[r]]), function(v) {
          regName <- names(dmat_convolved[[r]])[v]
          fname <- paste0(names(dmat_convolved)[r], "_", regName, ".1D")
          toWrite <- plyr::round_any(dmat_convolved[[r]][[v]], .000001)
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

    if ("FSL" %in% write_timing_files) {
      for (i in 1:dim(dmat)[1L]) {
        for (reg in 1:dim(dmat)[2L]) {
          regout <- dmat[[i,reg]][,c("onset", "duration", "value")]
          if (center_values && !all(regout[,"value"] == 0.0)) {
            #remove zero-value events from the regressor
            regout <- regout[regout[,"value"] != 0, ]

            #now mean center values (unless there is no variation, such as a task indicator function)
            if (sd(regout[,"value"]) > 0) { regout[,"value"] <- regout[,"value"] - mean(regout[,"value"]) }
          }

          fname <- paste0("run", runs_to_output[i], "_", dimnames(dmat)[[2L]][reg], "_FSL3col.txt")
          write.table(regout, file=file.path(output_directory, fname), sep="\t", eol="\n", col.names=FALSE, row.names=FALSE)
        }
      }
    }

    if ("AFNI" %in% write_timing_files) {
      #use dmBLOCK-style regressors: time*modulation:duration. One line per run

      #need to unify multiple values that share onset time
      #time*modulation1,modulation2:duration

      # regonsets <- lapply(dmat, function(reg) {
      #   reg[,"onset"]
      #   #unname(do.call(c, lapply(reg, function(run) { run[,"onset"]})))
      # })

      #this approach is flawed if the first onsets for a run for two events do not align because one is omitted.
      #for example, if there is no PE on trial 1, but then it is aligned later, the first onsets will differ spuriously
      #seems the only way around this might be to preserve trial as a field in dmat, then merge here.
      # regonsets <- apply(dmat, 1, function(run) {
      #   onsets <- sapply(run, function(df) { df[,"onset"]})
      #   rmat <- matrix(NA_real_, nrow=max(sapply(onsets, length)), ncol=length(onsets)) #use max length of any regressor as target
      #
      #   #unname(do.call(c, lapply(reg, function(run) { run[,"onset"]})))
      # })

      #TODO: make this work for uneven numbers of events across regressors
      #this will require some sort of outer_join approach using trial. I've now preserved trial as a field in dmat, but haven't solved this
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
    for (i in 1:length(dmat_convolved)) {
      #this is clunky, but necessary to make sure we grab the right nuisance signals (would need to refactor dmat_convolved to get it less clunky)
      runnum <- as.numeric(sub("run(\\d+)", "\\1", names(dmat_convolved)[i], perl=TRUE))
      nuisance_regressors_currun <- dplyr::filter(nuisance_regressors_df, run == runnum) %>% dplyr::select(-run)
      dmat_convolved[[i]] <- cbind(dmat_convolved[[i]], nuisance_regressors_currun)
    }
    #nuisance_regressors_df_split <- split(nuisance_regressors_df, nuisance_regressors_df$run)
    #dmat_changed <- lapply(dmat, function(x) {cbind(x, nuisance_regressors_df_split[[x]])})
    #dmat_convolved <- cbind(dmat_convolved, nuisance_regressors_df)
  }

  #compute collinearity diagnostics on the unconvolved signals
  collinearityDiag.raw <- apply(dmat, 1, function(run) {
    #custom regressors are not guaranteed to have ntrials as the dimension. Remove them from raw diagnostics since they're not on the same trial grid
    #rlengths <- sapply(run, length)
    custom_reg <- grep("^custom_", names(run))
    if (length(custom_reg) > 0L) { run <- run[-1*custom_reg]}

    #check correlations among regressors for trial-wise estimates
    #TODO: This also needs to support uneven numbers of events per regressor. Data.frame fails in this case
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

  #compute collinearity diagnostics on the convolved signals
  collinearityDiag.convolve <- lapply(dmat_convolved, function(run) {
    corvals <- cor(run, use="pairwise.complete.obs")
    vifMat <- data.frame(cbind(dummy=rnorm(nrow(run)), run)) #add dummy constant for vif
    vifForm <- as.formula(paste("dummy ~ 1 +", paste(names(run), collapse=" + ")))

    varInfl <- tryCatch(car::vif(lm(vifForm, data=vifMat)), error=function(e) { NA }) #return NA if failure
    list(r=corvals, vif=varInfl)
  })

  #add baseline terms to convolved design matrices
  if (baseline_coef_order > -1L) {
    dmat_convolved <- lapply(dmat_convolved, function(r) {
      n <- names(r)
      if (baseline_parameterization == "Legendre") {
        #consistent with AFNI approach, use Legendre polynomials, which are centered at zero to allow for appropriate baseline
        unnormalized.p.list <- legendre.polynomials( baseline_coef_order, normalized=FALSE )
        #evaluate polynomial between -1 and 1, which is where its desirable centered behavior exists.
        #although the functions are centered at 0, evaluating them on a given grid may lead to slight deviations from mean=0. Thus, center perfectly.
        baseline <- polynomial.values(polynomials=unnormalized.p.list, x=seq(-1,1, length.out=nrow(r)))
        baseline[2:length(baseline)] <- lapply(baseline[2:length(baseline)], function(v) { v - mean(v) }) #don't center constant!

        names(baseline) <- paste0("base", 0:baseline_coef_order)
        d <- cbind(r, baseline)
      } else if (baseline_parameterization == "orthogonal_polynomials") {
        #compute polynomials that are orthogonal to design effects
        #this may be somewhat dubious. for example, a linear trend in a task regressor would be preserved because it is given preference over baseline
        d <- data.frame(fmri.design(r, order=baseline_coef_order))
        names(d) <- c(n, paste0("base", 0:baseline_coef_order))
      } else {
        stop("Don't know how to handle baseline_parameterization:", baseline_parameterization)
      }
      d
    })
  }

  #concatenate regressors across runs by adding timing from MR files.
  runtiming <- cumsum(run_volumes)*tr #timing in seconds of the start of successive runs

  #Define concatenated onsets of all events (e.g., in an AFNI-style setup)
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

  to_return <- list(design=dmat, design_convolved=dmat_convolved, design_unconvolved=dmat_unconvolved, collin_raw=collinearityDiag.raw,
                   collin_convolve=collinearityDiag.convolve, concat_onsets=concat_onsets, run_volumes=run_volumes)

  to_return$design_plot <- visualize_design_matrix(concat_design_runs(to_return))

  if (plot == TRUE) { plot(to_return$design_plot) }
  return(to_return)

}



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
#   ev=list(value=rbind(data.frame(run=1, trial=1:50, value=rnorm(50)), data.frame(run=2, trial=1:50, value=rnorm(50))), event="cue", duration="custom_dur", normalization="evtmax_1"),
#   pe=list(value=rbind(data.frame(run=1, trial=1:50, value=rnorm(50)), data.frame(run=2, trial=1:50, value=rnorm(50))), event="outcome", duration=1)
# )
#
#
#
# df1 <- data.frame(csf = rnorm(100), wm = rnorm(100))
# df2 <- data.frame(csf = rnorm(121), wm = rnorm(121))
# #For events, BDM expects a single data frame that includes a column for the event type (e.g. outcome vs. cue), run number (1:n), trial number (nested within run, 1:x), onset, duration of event
# #potential custom duration if want to specify specific lengths of events
# # For signals, BDM expects a list of list. The first level of lists reflects each signal (e.g. pe vs value). In the second level, BDM expects value and event (e.g. cue vs outcome).
# #value is a data frame with run number (1:n), trial (1:x), and signal.
# #Additionally will also accept a third element to the list which can specify custom durations
# #Also BDM expects either a character vector of the function nifit for each run OR it expects a numerical vector for the volumes to be analyzed in each run
# #If you don't supply the run_volumes, character string it will infer the number of volumes based off of the last onset within the run and will add 12 ITIs to that by default (very risk; do not recommend!)
#
# #Test Case
#
# d <- build_design_matrix(events = events, signals = signals)
# dnocon <- build_design_matrix(events = events, signals = signals, convolve = FALSE)
# dcon <- d$design_convolved$run1
# dcon$time <- 1:nrow(dcon)
# ggplot(dcon, aes(x = time, y = pe)) + geom_line(size = 0.8)
# dnoconout <- dnocon$design_convolved$run1
# dnoconout$time <- 1:nrow(dcon)
# ggplot(dnoconout, aes(x = time, y = pe)) + geom_line(size = 0.8)
# dnuis <- build_design_matrix(events = events, signals = signals, nuisance_regressors = list(df1, df2), write_timing_files = c("AFNI", "FSL", "convolved"), baseline_coef_order = 2)
# dnuisrun1 <- dnuis$design_convolved$run1
# dnuisrun1$time <- 1:nrow(dnuisrun1)
# ggplot(dnuisrun1, aes(x = time, y = wm)) + geom_line(size = 0.8)
