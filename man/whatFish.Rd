\name{whatFish}
\alias{whatFish}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Names and codes for fish species}
\description{
Functions to convert the scientific name, English name or X3A codes of fish species and taxonomic groupings. 
}
\usage{
whatFish(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{a character vector of the Scientific name, the English name or the X3A code for the fish}
}
%\details{
%add details
%}
\value{

\code{whatFish} A function that returns a list of the the Scientific name, English name and the X3A code regardless of which of these it is passed. 

}

\author{Alastair Pout  \email{a.pout@marlab.ac.uk}}

\examples{  

whatFish("Gadus morhua")
whatFish("POK")                                    }
                                    
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

