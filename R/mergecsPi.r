#' @export
mergecsPi <- function(csobj){

if(class(csobj)%in%c("csPi")!=TRUE) stop("this function only works on a csPi object")


se <-slot(csobj,"se")
tr <-slot(csobj,"tr")
hh <-slot(csobj,"hh")
sl <-slot(csobj,"sl")
hl <-slot(csobj,"hl")
ca <-slot(csobj,"ca")



pktr <-apply(csobj@tr[,c("seCode", "year", "proj", "trpCode")],1,paste,collapse=".")
fkhh <-apply(csobj@hh[,c("seCode", "year", "proj", "trpCode")],1,paste,collapse=".")
#if(all(fkhh %in% pktr)){cat("all hh records have tr records \n") }
pkhh <-apply(csobj@hh[,c("seCode", "year", "proj", "trpCode", "staNum", "foId")],1,paste,collapse=".")
fkhl <-apply(csobj@hl[,c("seCode", "year", "proj", "trpCode", "staNum", "foId")],1,paste,collapse=".")
#if(all(fkhl %in% pkhh)){cat("all hl records have hh records \n") }

fkhltr <-apply(csobj@hl[,c("seCode", "year", "proj", "trpCode")],1,paste,collapse=".")



hlindex <-match(fkhl,pkhh)



csobj@hl$subRect <-csobj@hh$subRect[hlindex]
csobj@hl$rect <-csobj@hh$rect[hlindex]
csobj@hl$area <-csobj@hh$area[hlindex]
csobj@hl$foDate <-csobj@hh$foDate[hlindex]
csobj@hl$foCatNat <-csobj@hh$foCatNat[hlindex]
csobj@hl$foCatEu5 <-csobj@hh$foCatEu5[hlindex]
csobj@hl$foCatEu6 <-csobj@hh$foCatEu6[hlindex]
#csobj@hl$seCode <-csobj@hh$seCode[hlindex]
csobj@hl$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@hl$foDate)),2,3))
csobj@hl$month <-as.POSIXlt(csobj@hl$foDate)$mon+1

hlindex2 <-match(fkhltr,pktr)
csobj@hl$arvLoc <-csobj@tr$arvLoc[hlindex2]
csobj@hl$vslLen <-csobj@tr$vslLen[hlindex2]
csobj@hl$sampCtry <-csobj@tr$sampCtry[hlindex2]


seindex <-match(csobj@hl$seCode,csobj@se$seCode)
csobj@hl$sampStrata <-csobj@se$sampStrata[seindex]
csobj@hl$sampScheme <-csobj@se$sampScheme[seindex]

trindex <-match(pktr,fkhh)

csobj@tr$subRect <-csobj@hh$subRect[trindex]
csobj@tr$rect <-csobj@hh$rect[trindex]
csobj@tr$area <-csobj@hh$area[trindex]
csobj@tr$foDate <-csobj@hh$foDate[trindex]
csobj@tr$foCatNat <-csobj@hh$foCatNat[trindex]
csobj@tr$foCatEu5 <-csobj@hh$foCatEu5[trindex]
csobj@tr$foCatEu6 <-csobj@hh$foCatEu6[trindex]
csobj@tr$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@tr$foDate)),2,3))
csobj@tr$month <-as.POSIXlt(csobj@tr$foDate)$mon+1


if(dim(csobj)$ca[1]>0)
{


fkca <-apply(csobj@ca[,c("seCode", "year", "proj", "trpCode")],1,paste,collapse=".")

caindex <-match(fkca,pktr)
if(any(is.na(caindex))) warning("The key fields between tr and ca tables dont all match")
#csobj@ca$rect <-csobj@tr$rect[caindex]
#csobj@ca$area <-csobj@tr$area[caindex]
#csobj@ca$date <-csobj@tr$date[caindex]
csobj@ca$foCatNat <-csobj@tr$foCatNat[caindex]
csobj@ca$foCatEu5 <-csobj@tr$foCatEu5[caindex]
csobj@ca$foCatEu6 <-csobj@tr$foCatEu6[caindex]
#csobj@ca$quarter <-as.numeric(substr(quarters(as.POSIXlt(csobj@ca$date)),2,3))
#csobj@ca$yearfromdate <-as.numeric(substr(csobj@ca$date,1,4))
#csobj@ca$monthfromdate <-as.numeric(substr(csobj@ca$date,6,7))
csobj@ca$arvLoc <-csobj@tr$arvLoc[caindex]
csobj@ca$vslLen <-csobj@tr$vslLen[caindex]
csobj@ca$sampCtry <-csobj@tr$sampCtry[caindex]

seindex <-match(csobj@ca$seCode,csobj@se$seCode)
csobj@ca$sampStrata <-csobj@se$sampStrata[seindex]
csobj@ca$sampScheme <-csobj@se$sampScheme[seindex]


}
return(csobj)
}


