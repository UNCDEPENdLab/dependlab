% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_spm_contrasts.R
\name{generate_spm_contrasts}
\alias{generate_spm_contrasts}
\title{This function reads an SPM.mat file and generate contrasts based on the design matrix specification}
\usage{
generate_spm_contrasts(
  output_dir,
  condition_contrasts = TRUE,
  unit_contrasts = TRUE,
  effects_of_interest_F = TRUE,
  spm_path = "/gpfs/group/mnh5174/default/lab_resources/spm12",
  execute = FALSE
)
}
\arguments{
\item{output_dir}{location for SPM outputs and scripts for estimating contrasts. Must contain SPM.mat already}

\item{condition_contrasts}{see generate_spm_mat}

\item{unit_contrasts}{see generate_spm_mat}

\item{effects_of_interest_F}{see generate_spm_mat}

\item{spm_path}{see generate_spm_mat}

\item{execute}{whether to run contrast setup. This depends on SPM.mat having been created already. Default: FALSE}
}
\description{
This function reads an SPM.mat file and generate contrasts based on the design matrix specification
}
\author{
Michael Hallquist
}
