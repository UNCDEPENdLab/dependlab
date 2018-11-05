#' This function submits a single script using the qsub command.
#' It accepts a vector PBS arguments to be passed to the scheduler and
#' a vector of environment variables that should be passed to the compute node at job execution.
#'
#' The function returns the jobid of the scheduled job.
#'
#' @param script A script that should be executed by the PBS scheduler. This can contain PBS directives, but in the
#'      case of conflicts, the directives passed with \code{pbs_args} will take precedence.#'    
#' @param pbs_args A character vector of arguments to be included in the qsub command. Typically these
#'      will begin with '-l' such as '-l walltime=10:00:00'.
#' @param env_variables A named character vector containing environment variables and their values to be passed
#'      to the \code{script} at execution time using the '-v' directive. The names of this vector are the environment
#'      variable names and the values of the vector are the environment variable values to be passed in.
#' @param echo Whether to echo the qsub command to the terminal at the time it is scheduled. Default: TRUE.
#' @param fail_on_error Whether to stop execution of the script (TRUE), or issue a warning (FALSE) if the qsub fails.
#'      Defaults to FALSE (i.e., issue a warning).
#' 
#' @return A character string containing the jobid of the scheduled job.
#'
#' @examples
#' \dontrun{
#'   qsub_file('myscript.bash', pbs_args=c('-l walltime=10:00:00', '-l nodes=1:ppn=20'),
#'      env_variables=c(RUN_INDEX=2, MODEL_NAME='FSE21'))
#' }
#' 
#' @author Michael Hallquist
#' @export
qsub_file <- function(script, pbs_args=NULL, env_variables=NULL, echo=TRUE, fail_on_error=FALSE) {
  if (!is.null(pbs_args)) { pbs_args <- paste(pbs_args, collapse=" ") }
  if (!is.null(env_variables)) {
    env_variables <- paste("-v", paste(names(env_variables), env_variables, collapse=",", sep="="))
    pbs_args <- paste(pbs_args, env_variables)
  }
  
  stopifnot(file.exists(script))
  #qsubdir <- dirname(script)
  qsubstdout <- paste0(tempfile(), "_", tools::file_path_sans_ext(basename(script)), "_stdout") #use unique temp files to avoid parallel collisions in job tracking
  qsubstderr <- paste0(tempfile(), "_", tools::file_path_sans_ext(basename(script)), "_stderr")
  #setwd(qsubdir) #execute qsub from the temporary directory so that output files go there
  if (echo) { cat(paste("qsub", script, pbs_args), "\n") }
  jobres=system2("qsub", args=paste(script, pbs_args), stdout=qsubstdout, stderr=qsubstderr) #submit the qsub script and return the jobid

  jobid <- scan(file=qsubstdout, what="char", sep="\n", quiet=TRUE)
  joberr <- scan(file=qsubstderr, what="char", sep="\n", quiet=TRUE)

  if (jobres != 0) {
    jobid <- NULL
    if (fail_on_error) {
      stop("qsub submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
    } else {
      warning("qsub submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
    }
  }
  
  return(jobid)
}
