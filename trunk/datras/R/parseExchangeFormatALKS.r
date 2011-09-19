
#### Data to convert ICES Exchange format to Chrons, Length-frequencies and Age Length keys  #####

parseExchangeFormatALKs <- function (wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")  {

 setwd(wd)

fnames<-list.files()
fnames<-fnames[grep(".csv",fnames)]

#Age length keys data

out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
alk<-readLines(fnames[i])
alk<-cbind(alk,cnts)
alk<-alk[alk[,2]=="23",][,-2]
print(length(alk))
alk<-c(out,alk[-1])
}

alk <- data.frame(matrix(unlist(strsplit(alk,",")),byrow=T,ncol=23))

dimnames(alk)[[2]] <-c("recordtype","quarter","country","ship","gear","sweeplngt","gearexp","doortype","stno","haulno",
"year","speccodetype","speccode","areatype","areacode","lngtcode","lngtclass","sex","maturity","plusgr","age","noatalk","indwgt")

alk 

}

#Chrons
out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
nsibts<-readLines(fnames[i])
nsibts<-cbind(nsibts,cnts)
nsibts.chr<-nsibts[nsibts[,2]=="59",][,-2]
print(length(nsibts.chr))
if(i == 1){hds<-nsibts.chr[1]}
out<-c(out,nsibts.chr[-1])
}

#Dates

write(t(out),file="nsibts.chr")

out<-read.table("nsibts.chr",sep=",",header=F)
dimnames(out)[[2]]<-strsplit(hds,",")[[1]]

tdate<-paste(out$Year,"-",out$month,"-",out$Day,sep="")
hh<-substr(format(out$TimeShot),1,2)
hh[grep("  ",hh)] <- "00"
hh <- gsub(" ","0",hh)

mm<-  substr(format(out$TimeShot),3,4)
mm[grep("  ",mm)] <- "00"
mm <- gsub(" ","0",mm)
ttime<-paste(paste(hh,mm,sep=":"),":","00",sep="")
out$date<-paste(tdate,ttime)

write.table(out,file="nsibts.chr",sep=",",row.names=F,col.names=F)


#Bio
setwd("D:/bearedo/Database/DATRAS/NS-IBTS")

fnames<-list.files()
fnames<-fnames[grep(".csv",fnames)]

out<-NULL
for(i in 1:length(fnames)){
#i<-1
cnts<-count.fields(fnames[i],sep=",")
nsibts<-readLines(fnames[i])
nsibts<-cbind(nsibts,cnts)
nsibts.bio<-nsibts[nsibts[,2]=="24",][,-2]
print(length(nsibts.bio))
out<-c(out,nsibts.bio[-1])
}

write(t(out),file="nsibts.bio")
test<-read.table("nsibts.bio",sep=",")

 dimnames(lf)[[2]] <- c("recordtype","quarter","country","ship","gear","sweeplngt","gearexp","doortype",
"stno","haulno","year","speccodetype","speccode","specval","sex","totalno","catidentifier","nomeas","subfactor","subwgt","ascatcatchwgt",
"lngtcode","lngtclass","hlnoatlngt")
