\name{ASFIS}
\alias{ASFIS}
\docType{data}
\title{The FAO ASFIS species lists.}
\description{
Data frame of the ASFIS species lists as used by the FAO. 
}
\usage{
data(ASFIS)
}
\format{
Data frame consisting of columns: 

ISSCAAP:    The grouping code e.g. 32 = cod hakes haddocks, 38 = sharks rays, chimaeras;    

TAXOCODE:   Taxanomic code

X3A_CODE:   3 alpha code for the species e.g. MAC = mackerel, POK = saithe etc

Scientific_name:   Scientific name of the species

English_name:      English names for the species

French_name:      French name for the species

Spanish_name:      Spanish name for the species

Author:            Author for the species type specamine

Family:            Family to which the species belongs

Order:             Order to which the species belongs

Stats_data:         Species for which there is capture data held by FAO
}

\source{
  FAO as of Aug 2015
  
  \code{\link{http://www.fao.org/fishery/collection/asfis/en}}
}

\seealso{ 
#\code{\link{whatFish}}, function to map names and codes. 
}

\examples{
data(ASFIS)
head(ASFIS)
}
\keyword{datasets}
