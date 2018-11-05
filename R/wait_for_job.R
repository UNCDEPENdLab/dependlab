#' This function pauses execution of an R script while a scheduled qsub job is not yet complete.
#'
#' It is intended to give you control over job dependencies within R when the formal PBS
#' depend approach is insufficient, especially in the case of a script that spawns child jobs that
#' need to be scheduled or complete before the parent script should continue.
#'
#' @param jobid A valid id of an existing PBS job. (No validation on this argument at present)#' 
#' @param sleep_interval How often to recheck the job status, in seconds. Default: 10
#' @param max_wait How long to wait on the job before giving up, in seconds. Default: 24 hours
#' 
#' @return Nothing. Just returns when the blocking job completes.
#'
#' @examples
#' \dontrun{
#' wait_for_job('7968857.torque01.util.production.int.aci.ics.psu.edu')
#' }
#' 
#' @author Michael Hallquist
#' @export
wait_for_job <- function(jobid, sleep_interval=10, max_wait=60 * 60 * 24) {
  job_complete <- FALSE
  wait_total <- 0
  while(job_complete==FALSE) {
    status <- system(paste("qstat -f", jobid, "| grep -i 'job_state'"), intern=TRUE)
    job_state <- sub(".*job_state = ([A-z]).*", "\\1", status, perl=TRUE)
    if (wait_total > max_wait) {
      stop("Maximum wait time: ", max_wait, " exceeded. Stopping execution of parent script because something is wrong.")
    } else if (job_state == "C") {
      job_complete <- TRUE #drop out of this loop
    } else if (job_state == "R" || job_state == "Q") {
      cat("Job still running or queued: ", jobid, "\n")
      Sys.sleep(sleep_interval) #sleep 10 seconds before checking again
      wait_total <- wait_total + sleep_interval #track total time waiting
    } else {
      stop("Unknown job state for: ", jobid, ", state:", job_state, "\n")
    }      
  }
  
  return(invisible(NULL))
}
