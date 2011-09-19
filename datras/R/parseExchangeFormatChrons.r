
#### Data to extract Chronologicals from ICES Exchange format  #####


parseExchangeFormatChrons <- function (wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")  {

setwd(wd)

fnames<-list.files()
fnames<-fnames[grep("Exchange Data_",fnames)]
fnames<-fnames[grep(".csv",fnames)]


out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
chrons<-readLines(fnames[i])
chrons<-cbind(chrons,cnts)
chrons<-chrons[chrons[,2]=="59",][,-2]
print(length(chrons))
if(i == 1){hds<-chrons[1]}
out<-c(out,chrons[-1])
}

chrons <- data.frame(matrix(unlist(strsplit(out,",")),byrow=T,ncol=59))

dimnames(chrons)[[2]] <- tolower(strsplit(hds,",")[[1]])

#Dates


tdate<-paste(chrons$year,"-",chrons$month,"-",chrons$day,sep="")
hh<-substr(format(chrons$timeshot),1,2)
hh[grep("  ",hh)] <- "00"
hh <- gsub(" ","0",hh)

mm<-  substr(format(chrons$timeshot),3,4)
mm[grep("  ",mm)] <- "00"
mm <- gsub(" ","0",mm)
ttime<-paste(paste(hh,mm,sep=":"),":","00",sep="")
chrons$date<-as.POSIXct(paste(tdate,ttime),tz="GMT")

chrons

}


### Example ######

##chrons <-  parseExchangeFormatChrons(wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")
