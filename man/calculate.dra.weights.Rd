% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/risk.adjust.R
\name{calculate.dra.weights}
\alias{calculate.dra.weights}
\title{Generate weights for direct risk adjustment in each risk group reflecting how
likely they are to occur in the provided data. This will add a few columns to
input.dt, might bugger up if you have similarly named ones.}
\usage{
calculate.dra.weights(
  input.dt,
  group.col.name,
  reference.level = NULL,
  riskgp.col.name = "riskgp",
  weight.col.name = "wt",
  include.na = FALSE
)
}
\arguments{
\item{input.dt}{Input data.table with at least columns group.col.name and
riskgp.col.name.}

\item{group.col.name}{The name of the column that is being adjusted against.}

\item{reference.level}{The level of group.col.name that risk groups will be
adjusted to reflect. If NULL, it will be the population proportion
(Default: NULL).}

\item{riskgp.col.name}{The name of the risk group column (default: 'riskgp')}

\item{weight.col.name}{The name of the column in output that contains
weights (default: 'wt')}
}
\value{

}
\description{
Generate weights for direct risk adjustment in each risk group reflecting how
likely they are to occur in the provided data. This will add a few columns to
input.dt, might bugger up if you have similarly named ones.
}
