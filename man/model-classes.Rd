% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def_classes.R
\docType{class}
\name{model-classes}
\alias{model-classes}
\alias{baymedrSuperiority-class}
\alias{baymedrEquivalence-class}
\alias{baymedrNonInferiority-class}
\title{S4 classes to represent different models}
\description{
The S4 classes \linkS4class{baymedrSuperiority},
\linkS4class{baymedrEquivalence}, and \linkS4class{baymedrNonInferiority}
represent models for the superiority (\code{\link{super_bf}}), equivalence
(\code{\link{equiv_bf}}), and non-inferiority (\code{\link{infer_bf}}) tests,
respectively.
}
\section{Slots}{

\describe{
\item{\code{test}}{Type of test that was conducted.}

\item{\code{hypotheses}}{The hypotheses that are tested.}

\item{\code{data}}{The type of data that was used.}

\item{\code{prior_scale}}{The Cauchy prior scale that was used.}

\item{\code{bf}}{The resulting Bayes factor.}

\item{\code{interval}}{The equivalence interval in case of \code{\link{equiv_bf}}.}

\item{\code{ni_margin}}{The non-inferiority margin in case of \code{\link{infer_bf}}.}
}}

