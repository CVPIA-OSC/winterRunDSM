% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route.R
\name{route}
\alias{route}
\title{Route Natal Streams}
\source{
IP-117068
}
\usage{
route(
  year,
  month,
  juveniles,
  inchannel_habitat,
  floodplain_habitat,
  prop_pulse_flows,
  detour = NULL
)
}
\arguments{
\item{year}{The current simulation year, 1-20}

\item{month}{The current simulation month, 1-8}

\item{inchannel_habitat}{A vector of available habitat in square meters}

\item{floodplain_habitat}{A vector of available floodplain habitat in square meters}

\item{prop_pulse_flows}{The proportion of pulse flows}

\item{detour}{Values can be 'sutter' or 'yolo' if some juveniles are detoured on to that bypass, otherwise NULL}

\item{juvenile}{An n by 4 matrix of juvenile fish by watershed and size class}
}
\description{
Determines if juveniles stay in their natal tributary, are detoured
to a bypass, or out migrate during a simulated month
}
