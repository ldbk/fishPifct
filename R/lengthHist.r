#' @export
lengthHist <-function(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lengthHist
# function that plots histograms
# of the length frequency data
# in the hl table of csData objects
# Borrows heavily from MM's lenDisPlot code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(COSTeda)
#data(ASFIS)

if(class(x)%in%c("csPi","csData","csDataVal")==FALSE)stop("x is not csData")

spp <-ifelse(length(table(x@hl$spp))==1,x@hl$spp[1],"multiple species")
dots <-list(...)
if(class(x) %in%c("csPi"))
{
data(ASFIS)
object <-suppressWarnings(mergecsPi(x))@hl
}
if(class(x) %in%c("csData","csDataVal"))
{
data(code.list)
ASFIS <-code.list$spp
object <-suppressWarnings(mergecsData(x))@hl
}


if((by %in% names(object))!=TRUE)stop("by not a recognised grouping variable")
#if(all((fraction %in% names(table(object$catchCat)))==FALSE))
if(all(names(table(object$catchCat))=="DIS"))
{
fraction <-"DIS"
warning("Only DIS fraction present in data")
}
if(all(names(table(object$catchCat))=="LAN"))
{
fraction <-"LAN"
warning("Only LAN fraction present in data")
}
if(all(fraction %in% names(table(object$catchCat)))==FALSE)
{
stop(paste(fraction,"fraction not in the data"))
}

lgthCode <- as.character(x@sl$lenCode[1])
stepp <- c(1,5,10,25)
names(stepp) <- c("mm","scm","cm","25mm")
ste <- stepp[lgthCode]
varlevs <-as.character(level)
if(level[1]=="all")varlevs <-levels(factor(object[[by]]))
if(any(as.character(varlevs) %in% as.character(object[[by]])==FALSE))
{
stop("level not present in the variable")
}

df <-object
# a call to hist to get the breaks over the length class range
alllengths <-rep(df$lenCls,df$lenNum)
vals <-hist(alllengths,plot=F,breaks=seq(min(df$lenCls),max(df$lenCls),by=ste))
# running through the levels to get the ylimits
allcounts <-NULL
for(i in 1:length(varlevs))
{
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(dim(df)[1]==0)next
alllengths <-rep(df$lenCls,df$lenNum)
allcounts <-append(allcounts,hist(alllengths,breaks=seq(min(df$lenCls)
,max(df$lenCls),by=ste),plot=F)$counts)
}
# doing some defaults for the dots argument
addtitle <-FALSE
if(is.null(dots$main))addtitle <-TRUE
if(is.null(dots$axes))dots$axes <-TRUE
if(is.null(dots$add))dots$add <-FALSE
if(is.null(dots$angle))dots$angle <-45
if(is.null(dots$freq))dots$freq <-TRUE
if(is.null(dots$ylim))dots$ylim <-c(min(allcounts),max(allcounts))
if(dots$freq==FALSE)dots$ylim <-NULL
if(is.null(dots$xlab))dots$xlab <-"Length class (mm)"
if(is.null(dots$ylab)&dots$freq==TRUE)dots$ylab <-"Frequency"
if(is.null(dots$ylab)&dots$freq==FALSE)dots$ylab <-"Density"
# and finally plotting out the levels
out <-NULL
for(i in 1:length(varlevs))
{
if(addtitle)dots$main <-paste(by,varlevs[i],sep=" ")
if(addtitle&&by=="month")dots$main <-month.abb[as.numeric(varlevs)][i]
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]

alllengths <-rep(df$lenCls,df$lenNum)
out[[i]] <-hist(alllengths,main=dots$main,sub=dots$sub,xlab=dots$xlab,ylab=dots$ylab,breaks=vals$breaks
,ylim=dots$ylim,freq=dots$freq,col=dots$col,border=dots$border,
density=dots$density,angle=dots$angle,axes=dots$axes,add=dots$add,
cex.main=dots$cex.main
,line=dots$line,cex.axis=dots$cex.axis,cex.lab=dots$cex.lab)
}

# adding the outer margin title

fishname <-spp
if(fishname!="multiple species")
{
fishname <- as.character(ASFIS$English_name[ASFIS$Scientific_name == spp])
}
if(title)
{
title(paste("Length distribution for ",fishname," by ",by,

sep=""),outer=TRUE,line=-1,cex.main=dots$cex.main)
}
names(out) <-varlevs
invisible(out)
}





#------------end of lengthHist------------------------
