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
          signature("missing", "missing", "missing", "missing", "missing", "missing"), 
          function(se, tr, hh, sl, hl, ca, 
                   desc="Commercial Sampling Data", popData="Named cpRDB object", design="Design description", 
                   check=FALSE, ...) {
		  obj<-new("csPi")
		  return(obj)
	  }
	  )

#' @export
setMethod("csPi", 
          signature("data.frame", "data.frame", "data.frame", "data.frame", "data.frame", "data.frame"), 
          function(se, tr, hh, sl, hl, ca, 
                   desc="", popData="", design="",history="", 
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
  new("csPi", se=se,tr=tr, hh=hh, sl=sl, hl=hl, ca=ca, desc=desc, popData=popData, design=design,history=history)
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
setGeneric("export", function(object, filename, type,...){
		   	standardGeneric("export")
				}
)
#' @export
setMethod("export", signature(object="csPi",filename="character",type="character"), 
	  function(object,filename="output.xlsx",type="xlsx",...){
 		if(!(type%in%c("csv","xlsx","SQLite"))){type<-"notype"}
 			switch(type,
				csv={filename<-exportcsv(object,filename)
				},
        			xlsx={filename<-exportxlsx(object,filename)	
				},
				SQLite={filename<-exportdb(object,filename)
 				},
				notype={print("wrong output type, please use 'xlsx', 'csv' or 'SQLite'")
				}
			)
  		return(filename)
	}
)
setMethod("export", signature(object="csData",filename="character",type="character"), 
	  function(object,filename="output.xlsx",type="xlsx",...){
 		if(!(type%in%c("csv","xlsx","SQLite"))){type<-"notype"}
 			switch(type,
				csv={filename<-exportcsv(object,filename)
				},
        			xlsx={filename<-exportxlsx(object,filename)	
				},
				SQLite={filename<-exportdb(object,filename)
 				},
				notype={print("wrong output type, please use 'xlsx', 'csv' or 'SQLite'");filename<-""
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
	if(is.null(objslot)){
		print("Object has no slot, check your object. Nothing to export.")
		filename<-""
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
			con<-file(filenameslot,"wt")
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
#====================================================================
# 'import' function
#====================================================================
#' Import csPi or csData data from an excel file or csv file.
#'
#' @param file: an excel file name containing the csPi or csData tables, or a list of csv files.
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
  testcsPi<-all(namessheet%in%c("classVersion","desc","popData","design","history","se","tr","hh","sl","hl","ca"))
  testcsData<-all(namessheet%in%c("desc","tr","hh","sl","hl","ca"))
  if(testcsPi){
  	## read worksheets
  	classVersion<-readWorkbook(file, "classVersion")
  	desc<-readWorkbook(file, "desc")
  	popData<-readWorkbook(file, "popData")
  	design<-readWorkbook(file, "design")
  	history<-readWorkbook(file, "history")
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

#====================================================================
# 'importxlsx' function
#====================================================================
#' Import an excel file into a list 
#' Excel's sheets name defined list name slot
#'
#' @param filename: a file name.
#'
#' @return a list
#'
#' @author Norrent Bibroca & Laurbert Dullet 
#'
#' @export
importxlsx<-function(filename){
	#library(fishPifct)
	#filename<-"../vignettes/sole.xlsx"
	#get the sheet name
	namessheet<-getSheetNames(filename)
	pipo<-list()
	for(id in namessheet){
		pipo[[id]]<-readWorkbook(filename, id)
	}
	pipoPi<-csPi()
	nomslot<-slotNames(pipoPi)
	for(i in 1:length(nomslot)){
		tmp<-pipo[[which(names(pipo)%in%nomslot[i])]]
		if(ncol(tmp)==1){
			slot(pipoPi,nomslot[i])<-tmp[,1]
		}else{
			slot(pipoPi,nomslot[i])<-tmp
		}
	}
	return(pipoPi)
}
#====================================================================
# 'importcsv' function
#====================================================================
#' Import a list of csv file into a list 
#'
#' @param filelist: a character vector listing the files
#'
#' @return a list containing the object read sequentially
#'
#' @author Norrent Bibroca & Laurbert Dullet 
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  filelist<-export(sole.cs,file="output.csv",type="csv")
#'  pipo<-importcsv(file=filelist)
#'  str(pipoi,1)
#' }
#'
#' @export
importcsv<-function(filelist){
	pipo<-list()
	for(id in filelist){
		pipo[[id]]<-read.csv(id,stringsAsFactors=F)
	}
	return(pipo)
}
#====================================================================
# 'consistency' function
#====================================================================
#' Check the consistency between the slots of a csPi object
#'
#' @param obj: a csPi object
#'
#' @return a table 
#'
#' @author Laurent Dubroca
#'
#' @export
consistency<-function(obj){
	#library(fishPifct)
	#data(sole)
	#obj<- csDataTocsPi(sole.cs)
	if(class(obj)!="csPi"){
		print("This is not a csPi object. Consistency check aborted")
	}else{
		trse<-anti_join(obj@tr,obj@se,by=piPk("se"))
		rez<-data.frame(test="tr->se",message=paste0(nrow(trse)," tr records have no correspondings se records"),check=paste0("orphans tr trpCode:",paste(trse$trpCode,collapse=",")))
		hhtr<-anti_join(obj@hh,obj@tr,by=piPk("tr"))
		reztmp<-data.frame(test="hh->tr",message=paste0(nrow(hhtr)," hh records have no correspondings tr records"),check=paste0("orphans hh trpCode:",paste(hhtr$trpCode,collapse=",")))
		rez<-rbind(rez,reztmp)
		slhh<-anti_join(obj@sl,obj@hh,by=piPk("hh"))
		reztmp<-data.frame(test="sl->hh",message=paste0(nrow(slhh)," sl records have no correspondings hh records"),check=paste0("orphans sl trpCode:",paste(slhh$trpCode,collapse=",")))
		rez<-rbind(rez,reztmp)
		hlsl<-anti_join(obj@hl,obj@sl,by=piPk("sl"))
		reztmp<-data.frame(test="hl->sl",message=paste0(nrow(hlsl)," hl records have no correspondings sl records"),check=paste0("orphans hl trpCode:",paste(hlsl$trpCode,collapse=",")))
		rez<-rbind(rez,reztmp)
		return(rez)
	}
}
#====================================================================
# 'outliers' function
#====================================================================
#' Outliers detection on a variable using
#' the adjusted outlyingness index (based on the robustbase package) if
#' the variable is numeric, and on a frequency table if not. 
#' In the later case, a treshold in percentage
#' is used to flag an outlier (1% by default: if a value is present less than the
#' treshold, then it is reported as outlier).
#'
#' @param obj: a csPi object
#' @param slot: the slot where the variable of interest is located
#' @param var: the name of a variable on which outiers detection is applied
#' @param treshold: the percentage treshold to consider outliers in the case of non numeric data
#' @param other parameters related to the function adjOutlyingness from
#' 		the package robustbase
#'
#' @return plot a graphics and return a table of the table individuals flagged as outliers
#'
#' @author Laurent Dubroca
#'
#' @examples
#'\dontrun{
#'  data(sole)
#'  sole<-csDataTocsPi(sole.cs)
#'  outliers(sole,"foDur")
#' }
#'
#' @export
outliers<-function(obj,slot,var,treshold=1,...){
	#library(fishPifct)
	#data(sole)
	#obj<- csDataTocsPi(sole.cs)
	#var<-"foDur"
	#var<-"rect"
	#treshold<-1
	if(class(obj)!="csPi"){
		print("This is not a csPi object. Consistency check aborted")
	}else{
	   #for(whichslot in slotNames(obj)){
	   #	   if(any(names(slot(obj,whichslot))%in%var)){slotid<-whichslot}
	   #}
	   slotid<-slot
	   dat<-slot(obj,slotid)[,which(names(slot(obj,slotid))==var)]
	   if(is.numeric(dat)){
		   rez<-data.frame(id=1:length(dat),val=dat,outlier=NA)
		   datnona<-rez[!is.na(rez$val),]
		   out<-adjOutlyingness(datnona$val,...) 
		   rez$outlier[datnona$id]<-!out$nonOut
		   p1<-ggplot(rez,aes(x=val,fill=outlier))+geom_histogram(position="dodge")+xlab(paste(var,"in",slotid))
		   print(p1)
		   tab<-slot(obj,slotid)[rez$outlier & !is.na(rez$outlier),]
		   return(tab)
	   }else{
		   #print(paste(var,"in",slotid,"is not numeric."))
		   rez<-data.frame(id=1:length(dat),val=dat,outlier=FALSE)
		   rez<-rez%>%group_by(val)%>%mutate(n=n())%>%ungroup()%>%mutate(n=100*n/nrow(rez))
		   rez$outlier[rez$n<=treshold]<-TRUE
		   rez$outlier[is.na(rez$val)]<-NA
		   rezplot<-rez%>%group_by(val)%>%summarise(n=n(),outlier=unique(outlier))%>%ungroup()
		   p1<-ggplot(rezplot,aes(x=val,y=n,fill=outlier))+
		   	geom_bar(stat="identity")+
			xlab(paste(var,"in",slotid))
		   print(p1)
		   tab<-slot(obj,slotid)[rez$outlier & !is.na(rez$outlier),]
		   return(tab)
	   }
   	}
}
#a test
#options(defaul.stringsAsFactors=FALSE)

#GO
testfct<-function(){
library(fishPifct)
aa<-new("csPi")
str(aa@se)
data(sole)
pipo <- csDataTocsPi(sole.cs)
#test xlsx
filelist<-export(sole.cs,filename="output.xlsx",type="xlsx")
tt<-importxlsx(filelist)
filelist<-export(pipo,filename="output2.xlsx",type="xlsx")
#testcsv
filelist<-export(sole.cs,file="output.csv",type="csv")
tt<-importcsv(filelist$filename)
setClass(Class="test",slots=list(test1="character",test2="data.frame"),
				 prototype=list(test1="pipo",
						test2=data.frame(aa=numeric(),bb=character())))



testcsPi<-all(namessheet%in%c("info","se","tr","hh","sl","hl","ca"))

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

