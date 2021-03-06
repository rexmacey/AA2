% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cma.r
\name{createCMA}
\alias{createCMA}
\title{Create CMA (Capital Market Assumptions)}
\usage{
createCMA(rafiFN, acNamesFN, rafi_xlranges = NULL)
}
\arguments{
\item{rafiFN}{name including path of file with RAFI asset allocation assumptions}

\item{acNamesFN}{name including path of file with asset class characteristics}

\item{rafi_xlranges}{list of four ranges: return, corr, cov, and date}
}
\value{
a cma object
}
\description{
This function creates a cma object.
}
