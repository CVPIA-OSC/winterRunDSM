% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pulse_movement.R
\name{pulse_movement}
\alias{pulse_movement}
\title{Juvenile Pulse Movement}
\source{
IP-117068
}
\usage{
pulse_movement(
  proportion_pulse,
  .intercept = -7.70744,
  .proportion_pulse = 0.26579,
  .medium = 1.66845,
  .large = 0.5706,
  .vlarge = -4.305,
  .medium_pulse = -0.25477,
  .large_pulse = -0.44778,
  .very_large_pulse = 0.329
)
}
\arguments{
\item{proportion_pulse}{The proportion of flow that is a pulse, estimated
by the standard deviation of flow divided by the median flow for each month}

\item{.intercept}{Intercept, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.proportion_pulse}{Coefficient for the \code{proportion_pulse} variable, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.medium}{parameter for medium sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.large}{parameter for large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.vlarge}{parameter for very large sized fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.medium_pulse}{Additional coefficient for proportion_pulse variable for medium size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.large_pulse}{Additional coefficient for proportion_pulse variable for large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}

\item{.very_large_pulse}{Additional coefficient for proportion_pulse variable for very large size fish, source: Empirical model fit to CVPIA Chinook salmon screw trap abundance estimates from American River, Stanislaus River, Feather River, Mokelumne River, Sacramento River, Tuolumne River, Clear Creek 2008–2015.}
}
\description{
Calculates the proportion of juveniles in each size class that
migrate due to pulse flow
}
