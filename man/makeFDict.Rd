% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeDict.R
\name{makeFDict}
\alias{makeFDict}
\alias{parseF}
\title{Construct Feature Dictionary}
\usage{
makeFDict(fns = NULL)

parseF(fn)
}
\arguments{
\item{fns}{an option character vector of feature definition functions. This list is the basis of the feature dictionary. If \code{NULL} all functions which begins with \code{.dt} is included.}

\item{fn}{a feature definition function, either as string or actual function object.}
}
\description{
Construct Feature Dictionary
}
\details{
\code{parseF} works by scanning the body of the feature definition function (e.g. \code{body(.dtAIR)}), and look for any \code{joint}, \code{plane}, \code{fname}, and \code{featurename} specification.
}
\examples{
makeFDict()
makeFDict(c(".dtAIR",".dtDblBump"))

parseF(.dtAIR)
}
