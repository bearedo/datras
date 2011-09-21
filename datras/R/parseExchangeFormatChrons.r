
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


chrons$quarter <- as.numeric(as.character(chrons$quarter))
chrons$sweeplngt <- as.numeric(as.character(chrons$sweeplngt))
chrons$haulno <- as.numeric(as.character(chrons$haulno))
chrons$stno <- as.numeric(as.character(chrons$stno))
chrons$year <- as.numeric(as.character(chrons$year))
chrons$month <- as.numeric(as.character(chrons$month))
chrons$day <- as.numeric(as.character(chrons$day))
chrons$timeshot <- as.numeric(as.character(chrons$timeshot))
chrons$stratum <- as.numeric(as.character(chrons$stratum))
chrons$hauldur <- as.numeric(as.character(chrons$hauldur))
chrons$shootlat <- as.numeric(as.character(chrons$shootlat))
chrons$shootlong <- as.numeric(as.character(chrons$shootlong))
chrons$haullat <- as.numeric(as.character(chrons$haullat))
chrons$haullong <- as.numeric(as.character(chrons$haullong))
chrons$depth <- as.numeric(as.character(chrons$depth))
chrons$statrec <- as.character(chrons$statrec)
chrons$netopening <- as.numeric(as.character(chrons$netopening))
chrons$tickler <- as.numeric(as.character(chrons$tickler))
chrons$distance <- as.numeric(as.character(chrons$distance))
chrons$warplngt <- as.numeric(as.character(chrons$warplngt))
chrons$warpdia <- as.numeric(as.character(chrons$warpdia))
chrons$warpden <- as.numeric(as.character(chrons$warpden))
chrons$doorsurface <- as.numeric(as.character(chrons$doorsurface))
chrons$doorwgt <- as.numeric(as.character(chrons$doorwgt))
chrons$doorspread <- as.numeric(as.character(chrons$doorspread))
chrons$wingspread <- as.numeric(as.character(chrons$wingspread))
chrons$buoyancy <- as.numeric(as.character(chrons$buoyancy))
chrons$kitedim <- as.numeric(as.character(chrons$kitedim))
chrons$wgtgroundrope <- as.numeric(as.character(chrons$wgtgroundrope))
chrons$towdir <- as.numeric(as.character(chrons$towdir))
chrons$groundspeed <- as.numeric(as.character(chrons$groundspeed))
chrons$speedwater <- as.numeric(as.character(chrons$speedwater))
chrons$surcurdir <- as.numeric(as.character(chrons$surcurdir))
chrons$surcurspeed <- as.numeric(as.character(chrons$surcurspeed))
chrons$botcurdir <- as.numeric(as.character(chrons$botcurdir))
chrons$botcurdir <- as.numeric(as.character(chrons$botcurdir))
chrons$botcurspeed <- as.numeric(as.character(chrons$botcurspeed))
chrons$winddir <- as.numeric(as.character(chrons$winddir))
chrons$windspeed <- as.numeric(as.character(chrons$windspeed))
chrons$swelldir <- as.numeric(as.character(chrons$swelldir))
chrons$swellheight <- as.numeric(as.character(chrons$swellheight))
chrons$surtemp <- as.numeric(as.character(chrons$surtemp))
chrons$bottemp <- as.numeric(as.character(chrons$bottemp))
chrons$sursal <- as.numeric(as.character(chrons$sursal))
chrons$botsal <- as.numeric(as.character(chrons$botsal))
chrons$thermocline <- as.numeric(as.character(chrons$thermocline))
chrons$thclinedepth <- as.numeric(as.character(chrons$thclinedepth))
chrons$speedwater <- as.numeric(as.character(chrons$speedwater))
chrons$year <- as.numeric(as.character(chrons$year))

chrons$datim.shot<-as.POSIXct(paste(tdate,ttime),tz="GMT")
chrons$datim.haul<-chrons$datim.shot + chrons$hauldur *60   #Default is seconds

## Replace -9s with NAs

for ( i in c(8,9,10,11,12,13,14,18,19,20,21,28:59) )
{ chrons[,i] <- ifelse(chrons[,i]== -9, NA, chrons[,i]) }

print(str(chrons))


chrons

}







### Example ######

#chrons <-  parseExchangeFormatChrons(wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")
