agelenPlot <-function(x,by="spp",level="all",fraction=c("DIS","LAN"),title=TRUE,supsmu=FALSE,jitter=FALSE,...)
{
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# agelenPlot
# function that plots age given length
# of the ca table of csData objects
# Borrows heavily from MM's lenDisPlot code
# AP 22/04/09
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(COSTeda)
# data(ASFIS)

if(class(x)%in%c("csPi","csData","csDataVal")==FALSE)stop("x is not csPi, csData")

spp <-ifelse(length(table(x@ca$spp))==1,x@ca$spp[1],"multiple species")

dots <-list(...)
if(class(x) %in%c("csPi"))
{

data(ASFIS)
object <-suppressWarnings(mergecsPi(x))@ca

}
if(class(x) %in%c("csData","csDataVal"))
{
data(code.list)
ASFIS <-code.list$spp
object <-suppressWarnings(mergecsData(x))@ca
}

if((by %in% names(object))!=TRUE)stop("by not a recognised grouping variable")

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

lgthCode <- as.character(x@sl[,"lenCode"][1])
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
# running through the levels to get the ylimits
allcounts <-allages <-NULL
for(i in 1:length(varlevs))
{
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(dim(df)[1]==0)next
alllengths <-df$lenCls
strataage <-df$age

allcounts <-append(allcounts,alllengths)
allages <-append(allages,strataage)

}
# doing some defaults for the dots argument
addtitle <-FALSE
if(is.null(dots$add))dots$add <-FALSE
if(dots$add==TRUE&&is.null(dots$main)) dots$main <-""
if(is.null(dots$main))addtitle <-TRUE
if(is.null(dots$xlab))dots$xlab <-"Age"
if(is.null(dots$ylab))dots$ylab <-"Length class (mm)"
if(is.null(dots$axes))dots$axes <-TRUE
if(is.null(dots$col))dots$col <-rep(1,length(varlevs))
if(is.null(dots$col.line))dots$col.line <-dots$col
if(!is.null(dots$col)&length(dots$col==1))dots$col <-rep(dots$col,length(varlevs))
if(!is.null(dots$col.line)&length(dots$col.line==1))dots$col.line <-rep(dots$col.line,length(varlevs))


if(is.null(dots$pch))dots$pch <-rep(1,length(varlevs))
if(!is.null(dots$pch)&length(dots$pch==1))dots$pch <-rep(dots$pch,length(varlevs))
if(is.null(dots$lwd))dots$lwd <-rep(1,length(varlevs))
if(!is.null(dots$lwd)&length(dots$lwd==1))dots$lwd <-rep(dots$lwd,length(varlevs))
if(is.null(dots$lty))dots$lty <-rep(1,length(varlevs))
if(!is.null(dots$lty)&length(dots$lty==1))dots$lty <-rep(dots$lty,length(varlevs))



if(is.null(dots$ylim))dots$ylim <-c(min(allcounts,na.rm=T),max(allcounts,na.rm=T))
if(is.null(dots$xlim))dots$xlim <-c(min(allages,na.rm=T),max(allages,na.rm=T))
if(is.null(dots$span))dots$span <-"cv"
out <-NULL
# and finally plotting out the levels
for(i in 1:length(varlevs))
{

if(addtitle)dots$main <-paste(by,varlevs[i],sep=" ")
if(addtitle&&by=="month")dots$main <-month.abb[as.numeric(varlevs)][i]
df <- object[(as.character(object[[by]])%in%as.character(varlevs[i]))&(object$catchCat %in% fraction),]
if(jitter)jitterage <-jitter(df$age)
#alllengths <-df$lenCls
if(i==1|dots$add==FALSE)
{
plot(df$age,df$lenCls,ylim=dots$ylim,xlim=dots$xlim,main=dots$main,sub=dots$sub,col=dots$col[i],
pch=dots$pch[i],cex.main=dots$cex.main,cex.axis=dots$cex.axis,cex.lab=dots$cex.lab,
axes=dots$axes,xlab=dots$xlab,ylab=dots$ylab,cex=dots$cex)
if(jitter)points(jitterage,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(supsmu)suppressWarnings(lines(out[[i]] <-supsmu(df$age,df$lenCls,span=dots$span)
,col=dots$col.line[i],lwd=dots$lwd[i],lty=dots$lty[i]))
}

if(i>=2&dots$add==TRUE)
{
points(df$age,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(jitter)points(jitterage,df$lenCls,col=dots$col[i],pch=dots$pch[i],cex=dots$cex)
if(supsmu)suppressWarnings(lines(out[[i]] <-supsmu(df$age,df$lenCls,span=dots$span)
,col=dots$col.line[i],lwd=dots$lwd[i],lty=dots$lty[i]))


}


}

# adding the outer margin title

fishname <-spp
if(fishname!="multiple species")
{
fishname <- as.character(ASFIS$English_name[ASFIS$Scientific_name == spp])
}
if(title)
{
title(paste("Length given Age for ",fishname," by ",by,

sep=""),outer=TRUE,line=-1,cex.main=dots$cex.main)
}
if(!is.null(out))names(out) <-varlevs
invisible(out)
}

#--------------end of agelenPlot--------------------------------------------