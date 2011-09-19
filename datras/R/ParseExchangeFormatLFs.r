
#### Data to extract length frequencies from ICES Exchange format  #####


parseExchangeFormatLFs <- function (wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")  {

setwd(wd)

fnames<-list.files()
fnames<-fnames[grep("Exchange Data_",fnames)]
fnames<-fnames[grep(".csv",fnames)]


out<-NULL
for(i in 1:length(fnames)){
cnts<-count.fields(fnames[i],sep=",")
lf<-readLines(fnames[i])
lf<-cbind(lf,cnts)
lf<-lf[lf[,2]=="24",][,-2]
print(length(lf))
out<-c(out,lf[-1])
}

### Write data out and read it back in (temporarily) ###
write(t(out),file="lfs")
lf<-read.table("lfs",sep=",")
gc(reset=T)

system(command="del lfs")

dimnames(lf)[[2]] <- c("recordtype","quarter","country","ship","gear","sweeplngt","gearexp","doortype",
"stno","haulno","year","speccodetype","speccode","specval","sex","totalno","catidentifier","nomeas","subfactor","subwgt","ascatcatchwgt",
"lngtcode","lngtclass","hlnoatlngt")

lf

}


#### Example of use ####

lf <-  parseExchangeFormatLFs(wd =  "D:/bearedo/Database/DATRAS/NS-IBTS")


