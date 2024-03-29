% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finalimp.R
\name{finalimp}
\alias{finalimp}
\title{Wrapper for performing finalfit modelling using imputed data}
\usage{
finalimp(data, dependent, explanatory, random_effect = NULL)
}
\arguments{
\item{data}{Output from the impute() function.}

\item{dependent}{Character vector of length 1: quoted name of dependent variable. Can be continuous, a binary factor, or a survival object of form Surv(time, status).}

\item{explanatory}{Character vector of any length: quoted name(s) of explanatory variables.}

\item{random_effect}{Character vector of length 1, either, (1) name of random intercept variable, e.g. "var1", (automatically convered to "(1 | var1)"); or, (2) the full lme4 specification, e.g. "(var1 | var2)". Note parenthesis MUST be included in (2) but NOT included in (1).}
}
\value{
Nested list of (1) the final model table, and (2) the underlying data, models, and metrics for the original and imputed datasets.
}
\description{
Wrapper for performing finalfit modelling using imputed data
}
