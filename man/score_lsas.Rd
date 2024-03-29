% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_lsas.R
\name{score_lsas}
\alias{score_lsas}
\title{Score the Liebowitz Social Anxiety Scale (LSAS)}
\usage{
score_lsas(
  df,
  item_prefix = "LSAS_",
  max_impute = 0.2,
  min_value = 0,
  max_value = 3,
  drop_items = FALSE,
  add_alphas = TRUE
)
}
\arguments{
\item{df}{a data.frame containing the LSAS items to be scored}

\item{item_prefix}{a character prefix of the items names in \code{df} to be scored. Default: "LSAS_"}

\item{max_impute}{the proportion of missingness [0..1] or number [1..n] of missing values per subscale.
Below this, the mean will be imputed for missing items.}

\item{min_value}{the lowest value for the items anchors, used to check response validity. Default: 0}

\item{max_value}{the highest value for the items anchors, used to check response validity. Default: 3}

\item{drop_items}{whether to remove the item-level data from the \code{df}. Default: FALSE}

\item{add_alphas}{whether to compute coefficient alpha for subscales and return a column attribute. Default: TRUE}
}
\value{
A data frame with LSAS items, four social interaction & performance fear and avoidance subscales scores and two total fear and avoidance scores.
}
\description{
Score the Liebowitz Social Anxiety Scale (LSAS)
}
\author{
Nidhi Desai
}
