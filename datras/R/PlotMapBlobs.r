
plotMapBlobs <- function(input=cpue.dat,what.quarter=3,what.year = 2010,what.cpue='cpue.by.n',xlim0=c(-5,10),ylim0=c(45,62))
{

plot(1,1,type='n', xlim=xlim0,ylim=ylim0,xlab='',ylab='')
title(input$scientific.name[1])

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

ndatq <- input[input$quarter == what.quarter & input$year == what.year,]

#points(ndatq$Long[ndatq[,what.species] ==0],ndatq$Lat[ndatq[,what.species] ==0],pch='+',col='red', cex=0.5)

qq<-quantile(ndatq[,what.cpue],probs=seq(0,1,length=9))

legVals <- cut(ndatq[,what.cpue], breaks=qq,
labels=names(qq[-1]));

blobSize <- cut(ndatq[,what.cpue], breaks=qq, labels=as.character(0:7));
print(levels(blobSize))
blobSize <- as.numeric(as.character(blobSize));
#points(ndatr$Long,ndatr$Lat,cex=ndatq[,what.species]/scaling.factor,pch=16)

if(any(blobSize >= 1)){
points(ndatq$shootlong[blobSize > 0],ndatq$shootlat[blobSize > 0],cex=(blobSize[blobSize > 0]/3)+1,pch=21,col="black",bg='yellow')
}
if(any(blobSize < 1)){
points(ndatq$shootlong[blobSize < 1],ndatq$shootlat[blobSize < 1],cex=0.8,pch="+",col="black", bg="red")
}

qq1 <- round(qq,1)

qq2 <- c(paste(qq1[1],'-',qq1[2],sep=''),
paste(qq1[2],'-',qq1[3],sep=''),
paste(qq1[3],'-',qq1[4],sep=''),
paste(qq1[4],'-',qq1[5],sep=''),
paste(qq1[5],'-',qq1[6],sep=''),
paste(qq1[6],'-',qq1[7],sep=''),
paste(qq1[7],'-',qq1[8],sep=''),
paste(qq1[8],'-',qq1[9],sep=''))


legend("topright",legend=qq2,pch=c(3,rep(21,7)),pt.cex=((1:8)/3)+1,
bg='white',pt.bg=c('red',rep('yellow',7)),x.intersp=1.5,xjust=0.5,col='black',horiz=F,cex=.6,title=what.cpue)


}
