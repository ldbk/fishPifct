% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csDataTocsPi.r
\name{csDataTocsPi}
\alias{csDataTocsPi}
\title{Convert from COST CS data structure to fishPi CS structure.}
\usage{
csDataTocsPi(csObj, seObj = NULL, extra = FALSE)
}
\arguments{
\item{csObj}{COST CS data object to convert.}

\item{seObj}{SE (Sampling Event) table. If NULL a fake SE row is created and propagated.}

\item{extra}{Logical, must extra columns (e.g. not in the COST definition) be added to the fishPi CS structure.}
}
\value{
fishPi CS object.
}
\description{
Convert from COST CS data structure to fishPi CS structure.
}
\examples{
\dontrun{
data(sole)
pipo <- csDataTocsPi(sole.cs)
head(pipo)
}
}
\author{
Laurent Dubroca & Norbert Billet
}

