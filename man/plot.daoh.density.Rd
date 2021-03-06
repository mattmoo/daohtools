% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.daoh.density}
\alias{plot.daoh.density}
\title{Plot DAOH in a density chart}
\usage{
\method{plot}{daoh.density}(
  input.dt,
  daoh.col.name = "daoh",
  by.group = NA_character_,
  xlimits = c(-0.5, 90.5),
  adjust = 1.5
)
}
\arguments{
\item{input.dt}{The DAOH data (with daoh column)}

\item{daoh.col.name}{Name of DAOH column}

\item{by.group}{The group (string) by which to separate the histograms.}

\item{xlimits}{Length two numeric vector of xlimits..}
}
\value{
ggplot2 plot
}
\description{
Plot DAOH in a density chart
}
