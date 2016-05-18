whatFish <-function (x) 
{
    
data(ASFIS)

if(any(ASFIS$X3A_CODE %in% x))
{
   index <- which(ASFIS$X3A_CODE %in% x)
   index2 <- match(x, ASFIS$X3A_CODE[index])
   Sname <- as.character(ASFIS$Scientific_name[index[index2]])
Ename <- as.character(ASFIS$English[index[index2]])
Cname <-x
}
if(any(ASFIS$Scientific_name %in% x))
{ 
   index <- which(ASFIS$Scientific_name %in% x)
    index2 <- match(x, ASFIS$Scientific_name[index])
Cname <- as.character(ASFIS$X3A_CODE[index[index2]])
Ename <- as.character(ASFIS$English[index[index2]])
Sname <-x 
}

if(any(ASFIS$English_name %in% x))
{ 
   index <- which(ASFIS$English_name %in% x)
    index2 <- match(x, ASFIS$English_name[index])
    Cname <- as.character(ASFIS$X3A_CODE[index[index2]])
   Sname <- as.character(ASFIS$Scientific_name[index[index2]])
Ename <-x 
}

out <-list(Scientific=Sname,English=Ename,Code=Cname)
return(out)
}
