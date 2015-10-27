#' Subset a fishPi CS object.
#'
#' @param csObj fishPi CS object to subset.
#' @param subset expression.
#'
#' @return subseted fishPi CS object.
#'
#' @examples
#' \dontrun{
#' data(sole)
#' piSole.cs <- csDataTocsPi(sole.cs)
#' piSole.cs.sub <- csSubset(piSole.cs, daysAtSea > 5)
#' 
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
#' 
csSubset <- function(csObj, subset) {
  
  message("DEV VERSION: currently don't care about the ca table. NOT VALIDATED.")
  
  if (missing(subset)) {
    message("Missing subset. Original object returned.")
    return(csObj)
  }
  
  e <- substitute(subset)
  enclos <- parent.frame()
  e.names <- all.names(e, unique=TRUE)
  
  if (length(e.names)==0) {
    message("Non-pertinent subset. Original object returned.")
    return(csObj)
  }
  
  slots <- c("se", "tr", "hh", "sl", "hl")
  
  namesBySlots <- outer(e.names, slots, Vectorize(function(x, y) exists(x=as.character(x), where=slot(csObj, y), inherits=FALSE), SIMPLIFY=TRUE))
  dimnames(namesBySlots) <- list(as.character(e.names), slots)
  namesInSlots <- rowSums(namesBySlots) > 0
  
  recurseConcern <- function(x, slot) {
    if (is.atomic(x)) {
      return(FALSE)
    } else if (is.name(x)) {
      pos <- which.max(e.names==as.character(x))
      if (length(pos)==0) {
        return(FALSE)
      } else {
        return(namesBySlots[pos, slot])
      }
    } else if (is.call(x)) {
      return(any(sapply(x, recurseConcern, slot=slot)))
    } else {
      stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }
  
  recurseExists <- function(x, slot) {
    if (is.atomic(x)) {
      return(TRUE)
    } else if (is.name(x)) {
      pos <- which.max(e.names==as.character(x))
      if (length(pos)==0) {
        return(TRUE)
      } else {
        if (! namesInSlots[pos]) {
          return(TRUE)
        } else {
          return(namesBySlots[pos, slot])
        }
      }
    } else if (is.call(x)) {
      return(all(sapply(x, recurseExists, slot=slot)))
    } else {
      stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }
  
  recurseEval <- function(x, slot) {
    if (is.call(x)) {
      if (recurseExists(x, slot) & recurseConcern(x, slot)) {
        return(x)
      } else if (is.call(x)) {
        return(unlist(lapply(x, recurseEval, slot=slot)))
      }  
    }
  }
  
  builbTab <- function(slotName, expr, enclos) {
    tab <- slot(csObj, slotName)
    tab[, paste0(slotName,"RowInd")] <- seq_len(nrow(tab))
    calls <- unlist(recurseEval(expr, slotName))
    if (! is.null(calls)) {
      if (is.list(calls)) {
        r <- sapply(calls, eval, envir=tab, enclos=enclos)
        r <- apply(r, 1, any)
      } else {
        r <- eval(expr=calls, envir=tab, enclos=enclos)
      }
      tab <- tab[r, ]
    }
    tab <- tab[, unique(c(e.names[namesBySlots[, slotName]], piPk(slotName), paste0(slotName,"RowInd")))]
    return(tab)
  }
  
  for (curr in seq_len(length(slots)-1)) {
    y <- builbTab(slotName=slots[curr+1], expr=e, enclos=enclos)
    if (curr == 1) {
      x <- builbTab(slotName=slots[curr], expr=e, enclos=enclos)
      data <- merge(x=x, y=y, all.x=TRUE, by=piPk(slots[curr]))
    } else {
      data <- merge(x=data, y=y, all.x=TRUE, by=piPk(slots[curr]))
    }
  }
  
  r <- eval(expr=e, envir=data, enclos=enclos)
  data <- data[r & !is.na(r),]
  
  for(currSlot in slots) {
    slot(csObj, currSlot) <- slot(csObj, currSlot)[na.omit(unique(data[, paste0(currSlot,"RowInd")])), ]
  }
  
  return(csObj)
}