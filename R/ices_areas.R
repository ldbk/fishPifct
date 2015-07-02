#' ICES statistical area 
#'
#' ICES statistical area
#'
#' @format A spatial poygon data with 65 polygons (a data frame version 
#' suitable for ggplot is available, see example).
#' @source \url{http://www.ices.dk/marine-data/maps/Pages/ICES-statistical-rectangles.aspx}
#' @references \url{http://geo.ices.dk/download.php?dataset=ices_ref:ices_areas}
#' @examples
#'   \dontrun{
#'	require(ggplot2)
#' 	ices_areas_df<-fortify(ices_areas,region="ICES_area")
#'	ggplot(ices_area_df)+
#'		geom_polygon(aes(long,lat,group=group,fill=id))+
#'		guides(fill=guide_legend(ncol=3))
#'
#'   } 
#'
#' @name ices_areas
NULL

#' ICES statistical area 
#'
#' ICES statistical area
#'
#' @format A spatial poygon data with 65 polygons (a data frame version 
#' suitable for ggplot is available, see example).
#' @source \url{http://www.ices.dk/marine-data/maps/Pages/ICES-statistical-rectangles.aspx}
#' @references \url{http://geo.ices.dk/download.php?dataset=ices_ref:ices_areas}
#' @examples
#'   \dontrun{
#'	require(ggplot2)
#' 	ices_areas_df<-fortify(ices_areas,region="ICES_area")
#'	ggplot(ices_area_df)+
#'		geom_polygon(aes(long,lat,group=group,fill=id))+
#'		guides(fill=guide_legend(ncol=3))
#'
#'   } 
#'
#' @name ices_areas_df
NULL
