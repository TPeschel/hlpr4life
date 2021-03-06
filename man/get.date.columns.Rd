% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/life.tools.R
\name{get.date.columns}
\alias{get.date.columns}
\title{GET DATE COLUMNS}
\usage{
get.date.columns(data, pattern = "edat|date|datum", perl = T)
}
\arguments{
\item{data}{dataframe which has some date columns}

\item{pattern}{search pattern for finding column names via grep}

\item{perl}{logical: Use perl regex. Default is T.}
}
\value{
names of date columns
}
\description{
searches for some date like column names. one has the opportunity to give a
search string for a certain pattern.
}
\examples{
(d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",dat="2017-10-03",DATA="2017-10-02"))
get.date.columns(d)
(d<-data.frame(DATE="2017-10-05",EDAT="2017-10-04",MUTAD="2017-10-03"))
get.date.columns(d,"dat|muta")
}
