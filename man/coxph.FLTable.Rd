% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLCoxph.R
\name{coxph.FLTable}
\alias{coxph.FLTable}
\title{Cox Proportional Hazard Model}
\usage{
\method{coxph}{FLTable}(formula, data, ...)
}
\arguments{
\item{formula}{A symbolic description of model to fit}

\item{data}{FLTable object. Can be wide or deep}

\item{catToDummy}{Transform categorical variables to numerical values
either using dummy variables or by using Empirical
Logit. If the value is 1, transformation is done using
dummy variables, else if the value is 0,
transformation is done using Empirical Logit.}

\item{performNorm}{0/1 indicating whether to perform standardization of data.}

\item{performVarReduc}{0/1. If the value is 1,
the stored procedure eliminates variables based on standard deviation and
correlation.}

\item{makeDataSparse}{If 0,Retains zeroes and NULL values
from the input table. If 1, Removes zeroes and NULL. If 2,Removes zeroes 
but retains NULL values.}

\item{minStdDev}{Minimum acceptable standard deviation for
elimination of variables. Any variable that has a
standard deviation below this threshold is
eliminated. This parameter is only consequential if
the parameter PerformVarReduc = 1. Must be >0.}

\item{maxCorrel}{Maximum acceptable absolute correlation between
a pair of columns for eliminating variables. If the
absolute value of the correlation exceeds this
threshold, one of the columns is not transformed.
Again, this parameter is only consequential if the
parameter PerformVarReduc = 1. Must be >0 and <=1.}

\item{classSpec}{list describing the categorical dummy variables.}

\item{whereconditions}{takes the where_clause as a string.}
}
\value{
\code{coxph} performs linear regression and replicates equivalent R output.
}
\description{
\code{coxph} Fits a Cox proportional hazards regression model.
}
\section{Constraints}{

The formula object should have a \code{Surv} object.
The arguments to \code{Surv} object must strictly be in the order
(\code{time},\code{time2},\code{event}) or (\code{time},\code{event}).
Arguments to \code{Surv} should be plain. For instance, \code{as.numeric(event)}
inside \code{Surv} is not supported.
Only \code{coefficients},\code{linear.predictors},\code{FLSurvivalData},
\code{FLCoxPHStats},\code{loglik},\code{wald.test},\code{n},\code{nevent},
\code{rscore},\code{call},\code{formula},\code{call},\code{model},\code{x},
\code{means},\code{terms} can be called on fitted object using $.
coefficients,plot,print,summary methods are available for fitted object.
}
\examples{
library(RODBC)
connection <- odbcConnect("Gandalf")
widetable  <- FLTable("FL_DEMO", "siemenswideARDemoCoxPH", "ObsID")
fitT <- coxph(Surv(startDate,endDate,event)~meanTemp+age,widetable)
predData <- FLTable("FL_DEMO","preddatatoday","ObsID")
resultList <- predict(fitT,newdata=predData)
resultList[[1]]
resultList[[2]]
summary(fitT)
plot(fitT)
deeptable <- FLTable("FL_DEMO","siemensdeepARDemoCoxPH","obs_id_colname",
					"var_id_colname","cell_val_colname")
fitT <- coxph("",deeptable)
fitT$coefficients
summary(fitT)
plot(fitT)
}

