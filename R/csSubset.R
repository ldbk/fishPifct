#' Subset a fishPi CS object.
#'
#' @param csObj fishPi CS object to subset.
#' @param subset expression.
#' @param link: boolean TRUE if link to ca table is taken into account (merge of ca use only tr table)
#'
#' @return subseted fishPi CS object.
#'
#' @examples
#' \dontrun{
#' data(sole)
#' library(ggplot2)
#' sole.cs.pi <- csDataTocsPi(sole.cs) 
#' aggByFoCatEu5 <- csAggregate(csObj=sole.cs.pi, x=list(lenNum="lenNum"), by=list(foCatEu5="foCatEu5", lenCls="lenCls"), sum, na.rm=TRUE)
#' ggplot(data=aggByFoCatEu5) + geom_bar(mapping=aes(x=lenCls, y=lenNum), stat="identity") + facet_wrap(~foCatEu5, ncol=1, scales="free_y") + theme_bw()
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
#' 
csSubset <- function(csObj, subset,link=FALSE) {
  
  message("DEV VERSION: currently don't care about the ca table. NOT VALIDATED.\nLink option only use tr table to subset ca.")
  
  if (missing(subset)) {
    message("Missing subset. Original object returned.")
    return(csObj)
  }
  switch(class(csObj),
	         csData = funPk<-COSTPk,
	         csPi= funPk<-piPk,
		 )


  
  e <- substitute(subset)
  enclos <- parent.frame()
  e.names <- all.names(e, unique=TRUE)
  
  if (length(e.names)==0) {
    message("Non-pertinent subset. Original object returned.")
    return(csObj)
  }
  
  slots <- c("se", "tr", "hh", "sl", "hl")
  slots <- slotNames(csObj)
  slots<-slots[slots%in%c("se","tr","hh","sl","hl")]
 # print(slots)
  
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
    tab <- tab[, unique(c(e.names[namesBySlots[, slotName]], funPk(slotName), paste0(slotName,"RowInd")))]
    return(tab)
  }
  
  for (curr in seq_len(length(slots)-1)) {
    y <- builbTab(slotName=slots[curr+1], expr=e, enclos=enclos)
    if (curr == 1) {
      x <- builbTab(slotName=slots[curr], expr=e, enclos=enclos)
      data <- merge(x=x, y=y, all.x=TRUE, by=funPk(slots[curr]))
    } else {
      data <- merge(x=data, y=y, all.x=TRUE, by=funPk(slots[curr]))
    }
  }
  #test ca
  
  r <- eval(expr=e, envir=data, enclos=enclos)
  data <- data[r & !is.na(r),]

  
  for(currSlot in slots) {
    slot(csObj, currSlot) <- slot(csObj, currSlot)[na.omit(unique(data[, paste0(currSlot,"RowInd")])), ]
  }
  if(link){
      y<-slot(csObj,"ca")
      y$caRowInd<-seq_len(nrow(y))
      y <- y[, unique(c(funPk("ca"), "caRowInd"))]
      #y <- builbTab(slotName="ca", expr=e, enclos=enclos)
      dataca <- merge(x=data[,funPk("tr")], y=y, all.x=TRUE)#, by=funPk("hh"))
      r <- eval(expr=e, envir=dataca, enclos=enclos)
      dataca <- dataca[r & !is.na(r),]
      slot(csObj, "ca") <- slot(csObj, "ca")[na.omit(unique(dataca[, "caRowInd"])), ]
  }
  
  return(csObj)
}
