
plotMapBlobs <- function(input=cpue.dat,what.quarter=3,what.year = 2010,what.cpue='cpue.by.n',xlim0=c(-5,10),ylim0=c(45,62),scaling.factor=10)
{
#
#input=cpue.dat;
#what.quarter=3;what.year = 2007;
#what.cpue='cpue.by.n';
#xlim0=c(-15,10);
#ylim0=c(45,62);
#scaling.factor=1
##
plot(1,1,type='n', xlim=xlim0,ylim=ylim0,xlab='',ylab='')
title(paste(what.year," Q",what.quarter," ",input$scientific.name[1],sep=''))

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

ndatq <- input[!is.na(input$hlno) & input$quarter == what.quarter & input$year == what.year,]
ww <- (1:length(ndatq[,1]))[ndatq$hlno==0]
ndatq0 <- ndatq[ww,]  # the zeros
ndatq1 <- ndatq[-ww,] # the +ve component

qq<-seq(min(ndatq1[,what.cpue],na.rm=T),max(ndatq1[,what.cpue],na.rm=T),length=9)

ll <- length(qq)

legVals <- cut(ndatq1[,what.cpue], breaks=qq,labels=as.character(1:(ll-1)));

blobSize <- cut(ndatq1[,what.cpue], breaks=qq, labels=as.character(1:(ll-1)))
blobSize <- as.numeric(as.character(blobSize));

points(ndatq1$shootlong,ndatq1$shootlat,cex=(blobSize*scaling.factor),pch=21,col='black',bg='yellow')
points(ndatq0$shootlong,ndatq0$shootlat,pch=16,col='red')


legend("topright",
legend=round(c(0,seq(min(ndatq1[,what.cpue]),max(ndatq1[,what.cpue]),length=6)),2),
pch=c(16,rep(21,7)),
pt.cex=seq(min(blobSize,na.rm=T),max(blobSize,na.rm=T),length=7)*scaling.factor,
pt.bg=c('red',rep('yellow',6)),

bg='white',

x.intersp=1.5,xjust=0.5,col=c('red',rep('black',6)),horiz=F,cex=.6,title=what.cpue)


}

