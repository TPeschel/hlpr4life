% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/life.tools.R
\name{ifnot}
\alias{ifnot}
\title{IF NOT}
\usage{
ifnot(cond, optTrue, optFalse = { })
}
\arguments{
\item{cond}{condition that hast to be false}

\item{optTrue}{option running if condition is not TRUE}

\item{optFalse}{option running if condition is not FALSE}
}
\value{
execution of code either for optTrue or optFalse
}
\description{
short for if( !... ) do something else do something different
}
\examples{
ifnot( 1 == 2, cat( "one is not equal to two" ), cat( "one is not not equal to two" ) )
}
