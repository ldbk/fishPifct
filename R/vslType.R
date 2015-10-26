#' vslType 
#'
#' Vessel Types used for the DCF fleet segmentation and their repective DCF gear codes.  
#'
#' The DCF fleet segmantations are based on a vessel Type classification, 
#' these vessel types are in turn based on aggregations of DCF defined gear types. 
#' This data set enables vessel Types to be mapped from gaer types. 
#' Full details of the data can be obtained at 
#' \url{http://datacollection.jrc.ec.europa.eu/web/dcf/wordef/fleet-segment-dcf}.
#'
#' @format A data frame with 4 variables and 61 observations:
#' \itemize{
#'   \item  DCFGear: The DCF gaer code i.e. the first 3 characters of the metier code, 
#'   \item  NominalGearType: The DCF gear type description, 
#'   \item  vslType: the DCF vessel type appropriate to the gear type, 
#'   \item  GearType: Active or passive gear type classification. 
#' }
#' @source Alastair Pout a.pout@marlab.ac.uk and Work Package 2.2 core team.
#' @examples
#'\dontrun{
#'	# loading the data 
#'	data(vslType)
#'	# looking at the first 5 rows
#'	head(vslType)
#'} 
#'
#' @name vslType 
NULL
