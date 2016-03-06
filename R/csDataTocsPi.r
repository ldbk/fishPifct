#' Convert from COST CS data structure to fishPi CS structure.
#'
#' @param csObj COST CS data object to convert.
#' @param seObj SE (Sampling Event) table. If NULL a fake SE row is created and propagated.
#' @param extra Logical, must extra columns (e.g. not in the COST definition) be added to the fishPi CS structure.
#'
#' @return fishPi CS object.
#'
#' @examples
#' \dontrun{
#' data(sole)
#' pipo <- csDataTocsPi(sole.cs)
#' head(pipo)
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
csDataTocsPi <- function(csObj, seObj=NULL, extra=FALSE) {
  #------------------------------------------------------------------------
  # csDataTocsPi
  # function that converts csData objects into csPi objects
  # Makes a csPi object and populates the tr hh sl hl and ca tables
  # with those of the csObj
  # If provided with a second data frame will populate the su table with
  # the matching field names
  #------------------------------------------------------------------------
  
  x <- new("csPi")
  
  if (is.null(seObj)) {
    message("No seObj provided.")
    x@se[1,] <- NA
    x@se$seCode <- "autoGenerated"
  }
  
  if (!is.null(seObj)) {
    message("seObj added.")
    x@se <- seObj
    #x@se[1:dim(seObj)[1],match(names(seObj),names(x@se))] <-seObj[1:dim(seObj)[1],names(seObj)]
  }

  # to map the hh table we need to change field names from date and time
  names(csObj@hh)[which(names(csObj@hh)=="date")] <- "foDate"
  names(csObj@hh)[which(names(csObj@hh)=="time")] <- "foTime"

  for (currSlot in c("tr", "hh", "sl", "hl", "ca")) {
    if (nrow(slot(csObj, currSlot)) > 0) {
      id <- seq_len(nrow(slot(csObj, currSlot)))
      slot(x, currSlot) <- slot(x, currSlot)[id, ]
      row.names(slot(x, currSlot)) <- id
      commonNames <- which(names(slot(csObj, currSlot)) %in% names(slot(x, currSlot)))
      slot(x, currSlot)[, match(names(slot(csObj, currSlot))[commonNames], names(slot(x, currSlot)))] <- slot(csObj, currSlot)[, names(slot(csObj, currSlot))[commonNames]]
      slot(x, currSlot)$recType <- currSlot
      if (is.null(seObj)) slot(x, currSlot)$seCode <- "autoGenerated"
      if ((is.logical(extra) && extra) | (is.character(extra) && currSlot %in% extra)) {
        extraNames <- which(! names(slot(csObj, currSlot)) %in% names(slot(x, currSlot)))
        message("Extra column(s) added to \"", currSlot, "\" slot: ", paste0(names(slot(csObj, currSlot))[extraNames], collapse=", "), ".")
        slot(x, currSlot)[, names(slot(csObj, currSlot))[extraNames]] <- slot(csObj, currSlot)[, extraNames]
      }
      if (is.list(extra) && currSlot %in% names(extra)) {
        extraNames <- which(names(slot(csObj, currSlot)) %in% extra[[currSlot]])
        message("Extra column(s) added to \"", currSlot, "\" slot: ", paste0(names(slot(csObj, currSlot))[extraNames], collapse=", "), ".")
        slot(x, currSlot)[, names(slot(csObj, currSlot))[extraNames]] <- slot(csObj, currSlot)[, extraNames]
      }
    }
  }

  #if some variables are factors, convert them in character 
  #for(i in slotNames(x)){
#	  if(class(slot(x,i))=="data.frame"){
#  		idfactor <- sapply(slot(x,i), is.factor)
#  		slot(x,i)[idfactor] <- lapply(slot(x,i)[idfactor], as.character)
#	  }
#  }

  return(x)
}
