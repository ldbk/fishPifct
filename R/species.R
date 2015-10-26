#' Species names and WoRMS codes.
#'
#' The species listed here are 782 names and codes that are in the RDB as of June 2015 
#' and are accepted by the WoRMS list. WoRMS is the World Register of Marine Species.
#' See \url{http://www.marinespecies.org/}.
#'
#' @format A data frame with 2 variables and 782 observations:
#' \itemize{
#'   \item spp: the scientific species names, 
#'   \item code: the alphaID 6 digit code used by WoRMS.  
#' }
#' @source Alastair Pout a.pout@marlab.ac.uk and Work Package 2.2 core team.
#' @examples
#'\dontrun{
#'	# loading the data 
#'	data(species)
#'	# looking at the first 5 rows
#'	head(species)
#'} 
#'
#' @name species 
NULL
