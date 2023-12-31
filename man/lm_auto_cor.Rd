% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_auto_cor.R
\name{lm_auto_cor}
\alias{lm_auto_cor}
\title{lm_auto_cor:Batch build linear regression models with all possible combinations of covariates.}
\usage{
lm_auto_cor(outc, exp, cor, data, only_sig)
}
\arguments{
\item{outc}{outc should be a vector containing all outcome variables, such as c("A","B").}

\item{exp}{exp should be a vector containing all exposure variables, such as c("C","D").}

\item{cor}{cor should be a vector containing all covariates, such as c("E","F").}

\item{data}{data should be a data.frame containing all relevant variables.}

\item{only_sig}{only_sig should be TRUE or FALSE value.}
}
\description{
lm_auto_cor:Batch build linear regression models with all possible combinations of covariates.
}
