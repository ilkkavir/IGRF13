igrf <- function(date=c(2020,7,1),lat=70,lon=20,height=100,isv=0,itype=1){

#
# International geomagnetic reference field
#
#
#
#

  dyear <- date[1]
  ydays <- ifelse( date[1]%%4==0 , ifelse( date[1]%%100==0 , ifelse( date[1]%%400==0 , 366 , 365 ) , 366 ) , 365)
  sdays <- 0
  if(length(date)>1){
    mdays <- c(0,31,28,31,30,31,30,31,31,30,31,30,31)
    sdays <- sum(mdays[1:date[2]]) + ifelse(length(date)>2,(date[3]-1),0)
    if(date[2]>2){
      sdays <- sdays + ifelse( date[1]%%4==0 , ifelse( date[1]%%100==0 , ifelse( date[1]%%400==0 , 1 , 0 ) , 1 ) , 0)
    }
  }
  dyear <- dyear + sdays/ydays

  return(.Fortran("igrf13syn"               ,
                  isv   = as.integer(isv)   ,
                  date  = as.double(dyear)  ,
                  itype = as.integer(itype) ,
                  alt   = as.double(height) ,
                  colat = as.double(90-lat) ,
                  elong = as.double(lon)    ,
                  x     = as.double(0)      ,
                  y     = as.double(0)      ,
                  z     = as.double(0)      ,
                  f     = as.double(0)
    	    )
          )

} # igrf
