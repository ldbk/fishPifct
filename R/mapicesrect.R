#' This function map a variable defined on ICES statistical rectangle
#' 
#'
#' @param data: a data frame 
#' @param var: the variable position (a numerical vector) or name (a character vector) in data. This variable should be numeric.
#' @param rect: the ICES statistical rectangle position (a numerical vector) or name (a character vector) in data
#' 
#' @export
#' @return a ggplot plot
#' @keywords ICES statistical rectangle 
#' @examples
#'	\dontrun{
#'	require(mapdata);require(dplyr);require(ggplot2)
#'	load("ICESAreaRects.rdata")
	datatmp<-data.frame(StatRect=sample(gsub(" ","",ICESAreaRects$StatRect),1000,replace=T),landWt=rnorm(1000,mean=1000,sd=300),month=sample(1:12,1000,replace=T))
	pipo<-mapicesrect(datatmp[1:10,],2,1)
	listid<-unique(ices_areas_df$id)
	ggplot(ices_areas_df[ices_areas_df$id%in%listid[13:65],])+
		geom_path(aes(long,lat,group=group,coutour="grey"))

		geom_polygon(aes(long,lat,group=group,fill=id,coutour="grey"))#+guides(fill=guide_legend(ncol=3))

	ggplot(ices_areas_df[ices_areas_df$id%in%listid[1:12],],aes(x=long,y=lat,group=id))+
	geom_path()

#'   	} 
#'
#'   
mapicesrect<-function(data,var,rect){
	#trap input
	if(class(data)!="data.frame"){print("Data is not a data frame. Mapping stopped.");stop()}
	if(length(var)>1 | length(rect)>1){print("Length of var or rect != 1.");stop()}
	#check variable
	if(class(var)=="numeric"){
		if(var>ncol(data)){print(paste("The variable",var,"doesn't exist in data."));stop()}
		var0<-data[,var]
		var0name<-names(data)[var]
	}
	if(class(var)=="character"){
		if(!(var%in%names(data))){print(paste("The variable",var,"doesn't exist in data."));stop()}
		var0<-data[,which(var%in%names(data))]
		var0name<-names(data)[which(var%in%names(data))]
	}
	print(class(var0))
	if(!(class(var0)%in%c("numeric","integer"))){print(paste("The variable",var,"is not numeric."));stop()}
	#check ices rectangle 
	if(class(rect)=="numeric"){
		if(rect>ncol(data)){print(paste("The variable",rect,"doesn't exist in data."));stop()}
		rect0<-data[,rect]
	}
	if(class(rect)=="character"){
		if(!(rect%in%names(data))){print(paste("The variable",rect,"doesn't exist in data."));stop()}
		rect0<-data[,which(rect%in%names(data))]
	}
	#check if rectangle are ok
	if(!all(rect0%in%ICESAreaRects$StatRect)){
		print("Statistical rectangles discarded: ")
		whichnotok<-!(rect0%in%ICESAreaRects$StatRect)
		print(rect0[whichnotok])
	}
	#compute x and y from rect
	ices<-distinct(ICESAreaRects[,3:9]);ices$StatRect<-as.character(gsub(" ","",ices$StatRect))
	xytmp<-data.frame(StatRect=rect0,var0)
	#xytmp<-merge(xytmp,ices,all.x=T,all.y=F)
	xytmp<-left_join(xytmp,ICESAreaRects)
	#xytmp<-tbl_df(xytmp)%>%group_by(lon,lat)%>%summarise(landWt=sum(landWt,na.rm=T))
	#pipo<-tapply(xytmp$landWt,list(pipo$lon,pipo$lat),sum,na.rm=T)
	#xytmp[is.na(xytmp$landWt)]<-0
	coast_map <- fortify(map("worldHires", fill = TRUE, plot = FALSE,xlim=range(xytmp$lon,na.rm=T),ylim=range(xytmp$lat,na.rm=T)))
	map1<-ggplot(xytmp,aes(x=lon,y=lat,fill=var0))+
		geom_map(data=coast_map, map=coast_map, aes(x=long, y=lat, map_id=region), 
			 fill="grey", color="grey")+
		xlim(range(xytmp$lon,na.rm=T))+ylim(range(xytmp$lat,na.rm=T))+
		xlab("Longitude")+ylab("Latitude")+
		#geom_raster(stat="identity")+ 
		geom_tile(stat="identity")+ 
		coord_cartesian()+
		scale_fill_continuous(guide=guide_legend(title=var0name))+
		ggtitle(var0name)
	return(map1)
}
