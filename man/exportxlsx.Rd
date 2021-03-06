% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csPiUtils.R
\name{exportxlsx}
\alias{exportxlsx}
\title{Export a S4 object in an excel file.}
\usage{
exportxlsx(object, filename)
}
\arguments{
\item{object:}{a S4 object}

\item{file:}{a file name.}
}
\value{
file path of the generated file(s).
}
\description{
Export a S4 object in an excel file.
}
\examples{
\dontrun{
 data(sole)
 exportxlsx(sole.cs,file="sole.xlsx")
}

}
\author{
Norrent Bibroca & Laurbert Dullet
}

