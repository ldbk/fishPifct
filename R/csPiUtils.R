#====================================================================
# Class constructor
#====================================================================
# sets out a generic structure that the csData2 can take
# and then the setMethod functions each deal with specific cases, i.e. where some combination of the gerneric structure is missing
# there are 7 setMethod scenarios

setGeneric("csPi", 
           function(se, tr, hh, sl, hl, ca, ...) {
             standardGeneric("csPi")
           }
)

#' @export
setMethod("csPi", 
          signature("data.frame", "data.frame", "data.frame", "data.frame", "data.frame", "data.frame"), 
          function(se, tr, hh, sl, hl, ca, 
                   desc="Commercial Sampling Data", popData="Named cpRDB object", design="Design description", 
                   check=FALSE, ...) {
  # create object and name columns properly 
  obj <- new("csPi")
  # se
  se0 <- obj@se
  names(se) <- names(se0)
  #se <- coerceDataFrameColumns(se, se0)
  # tr
  tr0 <- obj@tr
  names(tr) <- names(tr0)
  #tr <- coerceDataFrameColumns(tr, tr0)
  # hh
  hh0 <- obj@hh
  names(hh) <- names(hh0)
  #hh <- coerceDataFrameColumns(hh, hh0)
  # sl
  sl0 <- obj@sl
  names(sl) <- names(sl0)
  #sl <- coerceDataFrameColumns(sl, sl0)
  # hl
  hl0 <- obj@hl
  names(hl) <- names(hl0)
  #hl <- coerceDataFrameColumns(hl, hl0)
  # ca
  ca0 <- obj@ca
  names(ca) <- names(ca0)
  #ca <- coerceDataFrameColumns(ca, ca0)
  
  #check
  #  if (check) check.fields(new("csData", tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc))
  # object
  new("csPi", se=se,tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc, popData=popData, design=design)
})




#===================================


#====================================================================
# 'Head' and 'Tail' functions
#====================================================================

setMethod("head", signature("csPi"), function(x, ...){
  object <- new("csPi",desc=x@desc,popData=x@popData,design=x@design)
  object@se <- head(x@se)
  object@tr <- head(x@tr)
  object@hh <- head(x@hh)
  object@sl <- head(x@sl)
  object@hl <- head(x@hl)
  object@ca <- head(x@ca)
  return(object)
}
)


setMethod("tail", signature("csPi"), function(x, ...){
  object <- new("csPi",desc=x@desc,popData=x@popData,design=x@design)
  object@se <- tail(x@se)
  object@tr <- tail(x@tr)
  object@hh <- tail(x@hh)
  object@sl <- tail(x@sl)
  object@hl <- tail(x@hl)
  object@ca <- tail(x@ca)
  return(object)
}
)

#====================================================================
# 'summary' function
#====================================================================

setMethod("summary", signature("csPi"), function(object, ...){
  ll <- list()
  ll$desc <- object@desc
  ll$popData <- object@popData
  ll$design <- object@design
  ll$se <-summary(object@se)
  ll$tr <- summary(object@tr)
  ll$hh <- summary(object@hh)
  ll$sl <- summary(object@sl)
  ll$hl <- summary(object@hl)
  ll$ca <- summary(object@ca)
  return(ll)
}
)


#====================================================================
# 'dim' function
#====================================================================

setMethod("dim", signature("csPi"), function(x){
  ll <- list()
  ll$se <-dim(x@se)
  ll$tr <- dim(x@tr)
  ll$hh <- dim(x@hh)
  ll$sl <- dim(x@sl)
  ll$hl <- dim(x@hl)
  ll$ca <- dim(x@ca)
  return(ll)
}
)

#====================================================================
# 'export' function
#====================================================================
#' Export csPi data in excel
#'
#' @param object: a csPi object.
#' @param file: a file name.
#'
#' @return file path of the generated file.
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  pipo<-csDataTocsPi(sole.cs)
#'  export(pipo,file="output.xslx")
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
setGeneric("export", function(object, file, ...){
		   	standardGeneric("export")
				}
)
#' @export
setMethod("export", signature("csPi"), function(object,file="csPi.xslx",...){
  wb <- createWorkbook()
  ## Add worksheets
  addWorksheet(wb, "info")
  addWorksheet(wb, "se")
  addWorksheet(wb, "tr")
  addWorksheet(wb, "hh")
  addWorksheet(wb, "sl")
  addWorksheet(wb, "hl")
  addWorksheet(wb, "ca")
  info<-data.frame(classVersion=object@classVersion,desc=object@desc,popData=object@popData,design=object@design,date=date())
  writeData(wb, "info", t(info), rowNames = TRUE,colNames=FALSE)
  writeData(wb, "se", object@se, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "tr", object@tr, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "hh", object@hh, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "sl", object@sl, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "hl", object@hl, startCol = 1, startRow = 1, rowNames = FALSE)
  writeData(wb, "ca", object@ca, startCol = 1, startRow = 1, rowNames = FALSE)
  saveWorkbook(wb, file, overwrite = TRUE)
  return(file)
}
)
#=================================


#====================================================================
# 'import' function
#====================================================================
#' Import csPi data from an excel file
#'
#' @param file: an excel file name containing the csPi table.
#'
#' @return a csPi object
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  pipo<-csDataTocsPi(sole.cs)
#'  export(pipo,file="output.xslx")
#' }
#' @export
#' @author Laurent Dubroca & Norbert Billet
import<-function(file){
  #get sheet names
  namessheet<-getSheetNames(file)
  if(!all(namessheet%in%c("info","se","tr","hh","sl","hl","ca"))){
	  "Sheet names problem"
  }else{
  	## read worksheets
  	info<-readWorkbook(file, "info")
  	se<-readWorkbook(file, "se")
  	tr<-readWorkbook(file, "tr")
  	hh<-readWorkbook(file, "hh")
  	sl<-readWorkbook(file, "sl")
 	hl<-readWorkbook(file, "hl")
  	ca<-readWorkbook(file, "ca")
  	dat<-csPi(se,tr,hh,sl,hl,ca,info[1,2],info[2,2],info[3,2])
  	return(dat)
  }
}
#=================================
