#' Export a csPi object in a SQLite database
#'
#' @param csObj: an object to import
#' @param file: the file name containing the SQLite database
#'
#' @return file name
#'
#' @examples
#'\dontrun{
#'	data(sole)
#'	csObj <- csDataTocsPi(sole.cs)
#'      exportdb(csObj,"csPi.db")
#'}
#' #
#' @author Norrent Bibroca & Laurbert Dullet 
#'
#' @export
exportdb<- function(csObj,file="csPi.db") {
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
	infoslot<-slotNames(csObj)
	tableslot<-infoslot[infoslot%in%c("se","tr","hh","hl","sl","ca")]
	infoslot<-infoslot[!infoslot%in%c("se","tr","hh","hl","sl","ca")]
	#write the table in it
	info<-slot(csObj,infoslot[1])
	if(length(infoslot)>1){
	 for(i in 2:length(infoslot)){
	  info<-c(info,slot(csObj,infoslot[i]))
	 }
	}
	info<-data.frame(t(info));names(info)<-infoslot
	dbWriteTable(con,"info", info)
	for(idslot in tableslot){
		dbWriteTable(con,idslot,slot(csObj,idslot))
	}
	dbDisconnect(con)
	print(paste0("SQLite db created in ",file))
	return(file)
	}
}

