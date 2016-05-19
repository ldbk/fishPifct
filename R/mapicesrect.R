#' This function map a variable from a slot from a csPi object 
#'
#' @param obj: a csPi object
#' @param slot: the slot name
#' @param var: the variable name 
#' @param type: the map type - tile or bubble. For categorical variable, only bubble is available.
#' @param fct: agregation function -sum, mean or n_distinct- to summarise the spatial information
#' 
#' @export
#' @return a ggplot plot
#' @keywords ICES statistical rectangle 
#' @author Laurent Dubroca
#' @examples
#'	\dontrun{
#'	library(fishPifct)
#'	data(sole)
#'	sole<- csDataTocsPi(sole.cs)
#'	csMap(sole,"hh","foDur","tile","sum")
#'   	} 
#'
#'   
csMap<-function(obj,slot,var,type="tile",fct="sum"){
	#library(fishPifct)
	#data(sole)
	#obj<- csDataTocsPi(sole.cs)
	#slot<-"tr"
	#var<-"trpCode"
	#fct<-"n_distinct"

	if(type%in%c("tile","bubble")){
	}else{
		print("map type not recognized: using tile")
		type<-"tile"
	}
	if(fct%in%c("sum","mean","n_distinct")){
	}else{
		print("summarizing function not recognized: using sum")
		fct<-"sum"
	}
	if(slot%in%c("ca","se")){
		print("ca and se slots are not available for mapping")
	}else{
		tab<-NULL
		if(slot=="hl"){
			tab<-left_join(left_join(obj@hl,obj@sl,by=piPk("sl")),obj@hh,by=piPk("hh"))
		}
		if(slot=="sl"){
			tab<-left_join(obj@sl,obj@hh,by=piPk("hh"))
		}
		if(slot=="hh"){
			tab<-obj@hh
		}
		if(slot=="tr"){
			tab<-left_join(obj@hh,obj@tr,by=piPk("tr"))
		}
		if(is.null(tab)){
			print("wrong slot name")
		}else{
			#agregated in space by sum for num and occurence for text 
			selvar<-slot(obj,slot)[,which(names(slot(obj,slot))%in%var)]
			if(is.numeric(selvar)){
				if(fct!="n_distinct"){
				  eval(parse(text=paste0("tab<-tab%>%group_by(rect)%>%summarise(valeur=",fct,"(",var,",na.rm=T))")))
				}else{
				  eval(parse(text=paste0("tab<-tab%>%group_by(rect)%>%summarise(valeur=",fct,"(",var,"))")))
				}
			}else{
				eval(parse(text=paste0("tab<-tab%>%group_by(rect,",var,")%>%summarise(val=n())")))
				names(tab)<-c("rect","cat","valeur")
			}
			
			#check if rectangle are ok
			if(!all(tab$rect%in%ICESAreaRects$StatRect)){
				print("Statistical rectangles missing and not mapped: ")
				whichnotok<-!(tab$rect%in%ICESAreaRects$StatRect)
				print(tab$rect[whichnotok])
			}
			#compute x and y from rect
			ices<-distinct(ICESAreaRects[,3:9]);ices$StatRect<-as.character(gsub(" ","",ices$StatRect))
			names(ices)[names(ices)=="StatRect"]<-"rect"
			tab<-left_join(tab,ices)%>%ungroup()%>%data.frame()
			#change spatial resolution ?
			#require(raster)
			#r<-rasterFromXYZ(data.frame(x=tab$lon,y=tab$lat,z=tab$val), res=c(NA,NA), crs=NA, digits=5)
			#facteur<-2
			#r<-aggregate(pipo,facteur)
			coast_map <- fortify(map("worldHires", fill = TRUE, plot = FALSE,
						 xlim=range(tab$lon,na.rm=T),ylim=range(tab$lat,na.rm=T)))
			

			#map
			if(is.numeric(selvar)){
				if(type=="tile"){
					map1<-ggplot(tab,aes(x=lon,y=lat,fill=valeur))+
						xlim(range(tab$lon,na.rm=T))+ylim(range(tab$lat,na.rm=T))+
						xlab("Longitude")+ylab("Latitude")+
						geom_tile(stat="identity")+ 
						geom_map(data=coast_map, map=coast_map, aes(x=long, y=lat, map_id=region), 
						 fill="grey", color="grey",alpha=.75)+
						coord_cartesian()+
						scale_fill_continuous(guide=guide_legend(title=var))+
						ggtitle(paste(fct,var,"in",slot))

				}
				if(type=="bubble"){
					map1<-ggplot(tab,aes(x=lon,y=lat,size=valeur))+
						xlim(range(tab$lon,na.rm=T))+ylim(range(tab$lat,na.rm=T))+
						xlab("Longitude")+ylab("Latitude")+
						geom_point() +
						geom_map(data=coast_map, map=coast_map, aes(x=long, y=lat, map_id=region), 
						 fill="grey", color="grey",inherit.aes=FALSE,alpha=.75)+
						coord_cartesian()+
						scale_fill_continuous(guide=guide_legend(title=var))+
						ggtitle(paste(fct,var,"in",slot))

				}
			}else{
				map1<-ggplot(tab,aes(x=lon,y=lat,size=valeur,color=cat))+
					geom_tile(data=tab,aes(x=lon,y=lat),alpha=0.01,inherit.aes=FALSE)+
					geom_map(data=coast_map, map=coast_map, aes(x=long, y=lat, map_id=region), 
					 fill="grey", color="grey",inherit.aes=FALSE,alpha=.75)+
					geom_jitter(width=1,height=0,alpha=.6)+
					xlim(range(tab$lon,na.rm=T))+ylim(range(tab$lat,na.rm=T))+
					xlab("Longitude")+ylab("Latitude")+
					#geom_raster(stat="identity")+ 
					#geom_tile(stat="identity")+ 
					coord_cartesian()+
					ggtitle(paste("number of",var,"in",slot))
			}
			
		}

	}
	return(map1)
}

#' This function map a variable defined on ICES statistical rectangles
#' 
#'
#' @param data: a data frame 
#' @param var: the variable position (a numerical vector) or name (a character vector) in data. This variable should be numeric.
#' @param rect: the ICES statistical rectangle position (a numerical vector) or name (a character vector) in data
#' 
#' @export
#' @return a ggplot plot
#' @keywords ICES statistical rectangle 
#' @author Laurent Dubroca
#' @examples
#'	\dontrun{
#'	load("ICESAreaRects.rdata")
#'	datatmp<-data.frame(StatRect=sample(gsub(" ","",ICESAreaRects$StatRect),1000,replace=T),landWt=rnorm(1000,mean=1000,sd=300),month=sample(1:12,1000,replace=T))
#'	pipo<-mapicesrect(datatmp[1:10,],2,1)
#'	listid<-unique(ices_areas_df$id)
#'	ggplot(ices_areas_df[ices_areas_df$id%in%listid[13:65],])+
#'		geom_path(aes(long,lat,group=group,coutour="grey"))
#'
#'		geom_polygon(aes(long,lat,group=group,fill=id,coutour="grey"))#+guides(fill=guide_legend(ncol=3))
#'
#'	ggplot(ices_areas_df[ices_areas_df$id%in%listid[1:12],],aes(x=long,y=lat,group=id))+
#'	geom_path()
#'
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
