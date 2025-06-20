% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_jmp_plus.R
\name{expiry}
\alias{expiry}
\title{Calculate expiration dates from all batches and models based on limits provided}
\usage{
expiry(data, lowerlimit, upperlimit, debug)
}
\arguments{
\item{data}{A result dataset of predicted values from the chosen model.}

\item{lowerlimit}{Lower limit to calculate lower confidence interval.}

\item{upperlimit}{Upper limit to calculate upper confidence interval.}

\item{debug}{Debugging. To turn on, debug = 1.}
}
\value{
Dataset containing estimated expiration date by model and batch.
}
\description{
Crossing time of 90\%CI used to get one-sided 95\%CI if only one limit is provided
Crossing time of 95\%CI used to get two-sided 95\%CI if both limits are provided
Crossing time is defined as the value just before it crosses the lower / upper limits
Expiry date for a batch / model is not missing if it is within 15 years of time 0
}
\examples{
  # Save confidence interval results from all models
  # results <- models(data = data, outcome = outcome, colours_named = colours_named, debug = debug)

  # Get expiry date of all batches from all models by number of limits provided
  # exp <- expiry(data = results[[2]], lowerlimit = 90, upperlimit = 110, debug = 0)
}
