#' This function compute vessel type according to metier and time spent fishing.
#' 
#' Predominant fishing gear as used in the DCF fleet segmentation:
#' http://datacollection.jrc.ec.europa.eu/wordef/fleet-segment-dcf.
#' This should be unique for each vessel. 
#' The dominance criteria shall be used to allocate each vessel to a segment based on the number of fishing days used
#' with each gear. If a fishing gear is used by more than the sum of all the others (i.e. a vessel spends more than 50 % of
#' its fishing time using that gear), the vessel shall be allocated to that segment. If not, the vessel shall be allocated to the
#' following fleet segment:
#' (a) vessels using Polyvalent active gears if it only uses active gears;
#' (b) vessels using Polyvalent passive gears if it only uses passive gears;
#' (c) vessels using active and passive gears.
#'
#' @param fishpidata fishPi dataset
#' 
#' @export
#' @return Vector of the computed vessel type
#' @keywords fishPi 
#' @examples
#'	\dontrun{
#'   	} 
#'
#'   
metier2vslType<-function(fishpidata){
	#internal function: character date to posix...
	fct1<-function(a1,a2){
		a1<-as.POSIXct(strptime(paste(a1,"12:00:00"),"%Y-%m-%d %H:%M:%S"))
		a2<-as.POSIXct(strptime(paste(a2,"12:00:00"),"%Y-%m-%d %H:%M:%S"))
		a12<-as.numeric(difftime(a2,a1,units="days"))
		a12<-a12+1
		return(a12)
	}
	#compute fishing days by vslId and gear from foCatEu6, and percentage of used gear by vslId
	fleetsegment<-fishpidata%>%ungroup()%>%transmute(vslId,gear=sub("_","",substr(foCatEu6,1,3)),daysfishing=fct1(depDate,arvDate)) %>%
		group_by(vslId,gear)%>%summarise(daysfishing=sum(daysfishing,na.rm=T)) %>%
		ungroup() %>%
		group_by(vslId)%>%mutate(totdays=sum(daysfishing,na.rm=T))%>%
		ungroup()%>%mutate(pdays=round(100*daysfishing/totdays,2))%>%
		group_by(vslId)%>%mutate(maxpdays=max(pdays,na.rm=T))
	#add type according to the segment definition and french metier occurence
	fleetsegment$gearnew<-str_replace_all(fleetsegment$gear,
		 c("GND"="DFN", 
		   "GTR"="DFN", 
		   "GTN"="DFN", 
		   "GNS"="DFN", 
		   "SDN"="DFN", 
		   "LN"="DFN", 
		   "FOO"="DRB", 
		   "OTB"="DTS", 
		   "OTT"="DTS", 
		   "PTB"="DTS", 
		   "PTM"="DTS", 
		   "TBB"="DTS", 
		   "LHP"="HOK", 
		   "LHM"="HOK", 
		   "LLS"="HOK", 
		   "LLD"="HOK", 
		   "LHP"="HOK",
		   "LTL"="HOK",
		   "REC"="HOK",
		   "LX"="HOK",
		   "SB"="PS",
		   "OTM"="TM",
		   "TBS"="TBB",
		   "FYK"="PGO",
		   "GNS"="PGO",
		   "GND"="PGO",
		   "GNC"="PGO",
		   "DIV"="MGO",
		   "FWR"="PGO",
		   "MIS"="PMP",
		   "OTH"="PMP"
		   ))
	#read the segment reference table
	reftablesegment<-reftabletype
	names(reftablesegment)[1]<-"gearnew"
	#if gear with maxpdays > 50% -> gear
	fleetsegmentsimple<-fleetsegment%>%ungroup()%>%filter(maxpdays>50)%>%filter(pdays==maxpdays)
	#if gear with maxpdays < 50% -> categorize 
	fleetsegmentcomplex<-fleetsegment%>%ungroup()%>%filter(maxpdays<=50)
	fleetsegmentcomplex<-left_join(fleetsegmentcomplex,reftablesegment)
	fleetsegmentcomplex<-fleetsegmentcomplex%>%ungroup()%>%
		group_by(vslId)%>%summarise(listgear=paste(unique(type),collapse=","))%>%
		mutate(test=ifelse(grepl(",",listgear),"mix",listgear))
	fleetsegmentcomplex$gearnew<-str_replace_all(fleetsegmentcomplex$test,
		 c("mix"="PMP", 
		   "passive"="PGP", 
		   "active"="MGP" 
		   ))
	fleetsegment<-rbind(data.frame(vslId=fleetsegmentsimple$vslId,vslType=fleetsegmentsimple$gearnew),
			    data.frame(vslId=fleetsegmentcomplex$vslId,vslType=fleetsegmentcomplex$gearnew))
	#fishpidata<-left_join(fishpidata,fleetsegment)
	return(fleetsegment)
}
