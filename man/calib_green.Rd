% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgreen.R
\name{calib_green}
\alias{calib_green}
\title{Calibration of the GREEN model}
\usage{
calib_green(catch_data, annual_data, n_iter, low, upp, years)
}
\arguments{
\item{catch_data}{data frame. Definition of the topological sequence of
catchments.}

\item{annual_data}{data frame. Sources of nutrient for each year and
catchments.}

\item{n_iter}{numeric. Number of iterations for the calibration process.}

\item{low}{numeric. Lower bounds of the calibration parameters.}

\item{upp}{numeric. Upper bounds of the calibration parameters.}

\item{years}{integer. Years to be used in the calibration. For sequences use
c(yearini:yearend).}
}
\value{
One object, a data frame with the model calibration
}
\description{
Runs GREEN model calibration
}
\examples{
\donttest{
# the data of the TN scenario
data(catch_data_TN)
data(annual_data_TN)
# the parameter for the calibration of the model
n_iter <- 2 # number of iterations
# the lower limits for all params (alpha_P, alpha_L, sd_coef)
low <- c(10, 0.000, 0.1)
# the upper limits for all params (alpha_P, alpha_L, sd_coef)
upp <- c(70, 0.3,  0.9)
# years in which the model should be executed
years <- 1990:2018
# execution of the calibration
dF_calib <- calib_green(catch_data_TN, annual_data_TN, n_iter, low, upp,
years)
}

}
