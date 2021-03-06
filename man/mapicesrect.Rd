% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapicesrect.R
\name{mapicesrect}
\alias{mapicesrect}
\title{This function map a variable defined on ICES statistical rectangles}
\usage{
mapicesrect(data, var, rect)
}
\arguments{
\item{data:}{a data frame}

\item{var:}{the variable position (a numerical vector) or name (a character vector) in data. This variable should be numeric.}

\item{rect:}{the ICES statistical rectangle position (a numerical vector) or name (a character vector) in data}
}
\value{
a ggplot plot
}
\description{
This function map a variable defined on ICES statistical rectangles
}
\examples{
\dontrun{
load("ICESAreaRects.rdata")
datatmp<-data.frame(StatRect=sample(gsub(" ","",ICESAreaRects$StatRect),1000,replace=T),landWt=rnorm(1000,mean=1000,sd=300),month=sample(1:12,1000,replace=T))
pipo<-mapicesrect(datatmp[1:10,],2,1)
listid<-unique(ices_areas_df$id)
ggplot(ices_areas_df[ices_areas_df$id\%in\%listid[13:65],])+
	geom_path(aes(long,lat,group=group,coutour="grey"))

	geom_polygon(aes(long,lat,group=group,fill=id,coutour="grey"))#+guides(fill=guide_legend(ncol=3))

ggplot(ices_areas_df[ices_areas_df$id\%in\%listid[1:12],],aes(x=long,y=lat,group=id))+
geom_path()

  	} 

  
}
\author{
Laurent Dubroca
}
\keyword{ICES}
\keyword{rectangle}
\keyword{statistical}

