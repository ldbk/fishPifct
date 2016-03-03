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
#' Export csPi or csData object in excel, csv or SQLite files.
#'
#' @param object: a csPi or csData object.
#' @param file: a file name.
#' @param type: "csv" or "xlsx" or "SQLite"
#'
#' @return file path of the generated file(s).
#'
#' @seealso \code{link{exportxlsx}}, \code{link{exportcsv}} and \code{link{exportdb}} 
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  #xlsx export
#'  export(sole.cs,file="sole.xlsx")
#'  #csv export
#'  export(sole.cs,file="sole.csv")
#'  #SQLite export
#'  export(sole.cs,file="sole.sqlite3")
#' }
#'
#' @author Norrent Bibroca & Laurbert Dullet 
#'
#' @export
setGeneric("export", function(object, file, type,...){
		   	standardGeneric("export")
				}
)
#' @export
setMethod("export", signature("csPi","csData"), function(object,file="output.xlsx",type="xlsx",...){
 if(!(type%in%c("csv","xlsx","SQLite"))){type<-"notype"}
 switch(type,
	csv={filename<-exportcsv(object,file)
	},
        xlsx={filename<-exportxslx(object,file)	
	},
	SQLite={filename<-exportdb(object,file)
 	},
	notype={print("wrong output type, please use 'xlsx', 'csv' or 'SQLite'")
	}
	)
  	return(filename)
}
)
#====================================================================
# 'exportxlsx' function
#====================================================================
#' Export a S4 object in an excel file.
#'
#' @param object: a S4 object
#' @param file: a file name.
#'
#' @return file path of the generated file(s).
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  exportxlsx(sole.cs,file="sole.xlsx")
#' }
#'
#' @author Norrent Bibroca & Laurbert Dullet 
#'
#' @export
exportxlsx<-function(object,filename){
	objslot<-slotNames(object)
	listmainslot<-
	if(is.null(objslot)){
		print("Object has no slot, check your object. Nothing to export.")
	}else{
		#generate wb
  		wb <- createWorkbook()
  		## Add worksheets and data
		for(i in 1:length(objslot)){
			addWorksheet(wb,objslot[i])
			slottmp<-slot(object,objslot[i])
			if(class(slottmp)=="character"){slottmp<-data.frame(slottmp);names(slottmp)<-as.character(objslot[i])}
  			#eval(parse(text=paste0("writeData(wb,'",objslot[i],"',object@",objslot[i],")")))
  			writeData(wb,objslot[i],slottmp)
		}
		#saving the workbook
  		saveWorkbook(wb, filename, overwrite = TRUE)
	}
	return(filename)
}
#====================================================================
# 'exportcsv' function
#====================================================================
#' Export a S4 object in multiple csv files.
#' The csv files are generated using the name provided and adding the object slotname in the name between the name
#' and the file extension (usually '.csv'). If the file name provided has no extension, '.csv' is added to the name.
#'
#' @param object: a S4 object
#' @param file: a base for the file names.
#'
#' @return files path of the generated files.
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  exportcsv(sole.cs,file="sole.csv")
#' }
#'
#' @author Naurrent Bibrolet & Lorbert Dullca 
#'
#' @export
exportcsv<-function(object,filename){
	objslot<-slotNames(object)
	listfilename<-data.frame()
	if(is.null(objslot)){
		print("Object has no slot, check your object. Nothing to export.")
	}else{
		#generate wb
		for(i in 1:length(objslot)){
			dotpos<-max(gregexpr("\\.",filename)[[1]])
			if(dotpos!=-1){
				filenameslot<-paste0(substr(filename,1,dotpos-1),"_",objslot[i],substr(filename,dotpos,nchar(filename)))
			}else{
				filenameslot<-paste0(filename,"_",objslot[i],".csv")
			}
			listfilename<-rbind(listfilename,filenameslot)
			slottmp<-slot(object,objslot[i])
			if(class(slottmp)=="character"){slottmp<-data.frame(slottmp);names(slottmp)<-as.character(objslot[i])}
			con<-file(filenameslot,"at")
			write.csv(slottmp,con,row.names=F)
			close(con)
		}
	}
	names(listfilename)<-"filename"
	return(listfilename)
}
#====================================================================
# 'exportdb' function
#====================================================================
#' Export an S4 object to a SQLite database.
#'
#' @param object: an S4 object
#' @param file: a file name.
#'
#' @return file path of the generated file.
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  exportdb(sole.cs,file="sole.sqlite3")
#' }
#'
#' @author Nobent Dibroca & Laurrert Bullet 
#'
#' @export
exportdb<- function(object,file) {
	#data(sole)
	#pipo <- csDataTocsPi(sole.cs)
	#csObj<-pipo
	if(!require(RSQLite)){
		   print("RSQLite package needed")
	}else{
	require(RSQLite)
	#file<-"sqlite.db"
	# Create a database in memory.
	con <- dbConnect(SQLite(),dbname=file)#, ":memory:")
	#select slot
	slotlist<-slotNames(object)
	for(idslot in slotlist){
		slottmp<-slot(object,idslot)
		if(class(slottmp)=="character"){slottmp<-data.frame(slottmp);names(slottmp)<-as.character(idslot)}
		dbWriteTable(con,idslot,slottmp,overwrite=TRUE)
	}
	dbDisconnect(con)
	print(paste0("SQLite db created in ",file))
	return(file)
	}
}

#test area
#library(fishPifct)
#data(sole.cs)
### Not run: 
#data(sole)
#pipo <- csDataTocsPi(sole.cs)
#head(pipo)
#object<-pipo
#filename<-"test.csv"
#exportxlsx(sole.cs,"test.xlsx")
#exportxlsx(pipo,"test.xlsx")
#exportcsv(pipo,"test2")
#exportcsv(pipo,"te.st2.adada")
#exportdb(sole.cs,"pipo.sqlite3")



#COSTcore format
setMethod("export", signature("csData"), function(object,file="csData.xlsx",...){
  wb <- createWorkbook()
  ## Add worksheets
  addWorksheet(wb, "desc")
  addWorksheet(wb, "tr")
  addWorksheet(wb, "hh")
  addWorksheet(wb, "sl")
  addWorksheet(wb, "hl")
  addWorksheet(wb, "ca")
  writeData(wb, "desc", object@desc, rowNames = TRUE,colNames=FALSE)
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
#' Import csPi or csData data from an excel file.
#'
#' @param file: an excel file name containing the csPi or csData tables.
#'
#' @return a csPi or a csData object
#'
#' @examples
#'\dontrun{
#'  #test with csData
#'  data(sole)
#'  export(sole.cs,file="output.xlsx")
#'  pipo<-import(file="output.xlsx")
#'  head(pipo)
#'  #test with csPi
#'  data(sole)
#'  pipo<-csDataTocsPi(sole.cs)
#'  export(pipo,file="output.xlsx")
#'  pipo<-import(file="output.xlsx")
#'  head(pipo)
#' }
#' @export
#' @author Norbert Billet & Laurent Dubroca
import<-function(file){
#	file<-"/home/moi/output.xlsx"
  #get sheet names
  namessheet<-getSheetNames(file)
  testcsPi<-all(namessheet%in%c("info","se","tr","hh","sl","hl","ca"))
  testcsData<-all(namessheet%in%c("desc","tr","hh","sl","hl","ca"))
  if(testcsPi){
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
  if(testcsData){
	  if(!require(COSTcore)){
		  print("You need COSTcore package to handle csData object")
	  }else{
		require(COSTcore)
  		## read worksheets
  		desc<-readWorkbook(file, "desc",colNames=FALSE)
  		tr<-readWorkbook(file, "tr")
  		hh<-readWorkbook(file, "hh")
  		sl<-readWorkbook(file, "sl")
 		hl<-readWorkbook(file, "hl")
  		ca<-readWorkbook(file, "ca")
  		dat<-csData(tr,hh,sl,hl,ca)
		dat@desc<-as.character(desc)
  		return(dat)
	  }
  }
  if(!testcsPi & !testcsData){
	  print("Sheet names problem")
  }
}
#=================================
