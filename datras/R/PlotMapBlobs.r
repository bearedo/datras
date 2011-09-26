
plotMapBlobs <- function(input=cpue.dat,what.quarter=3,what.year = 2010,what.cpue='cpue.by.n',xlim0=c(-5,10),ylim0=c(45,62),scaling.factor=10)
{

#input=cpue.dat;
#what.quarter=3;what.year = 2009;
#what.cpue='cpue.by.n';
#xlim0=c(-15,5);
#ylim0=c(45,62);
#scaling.factor=.5

plot(1,1,type='n', xlim=xlim0,ylim=ylim0,xlab='',ylab='')
title(input$scientific.name[1])

map("worldHires", add=TRUE, col='darkseagreen', fill=TRUE, bg="white",
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))

ndatq <- input[input$quarter == what.quarter & input$year == what.year,]

qq<-seq(-.01,max(ndatq[,what.cpue]),length=9)
ll <- length(qq)

legVals <- cut(ndatq[,what.cpue], breaks=qq,labels=as.character(0:(ll-2)));

blobSize <- cut(ndatq[,what.cpue], breaks=qq, labels=as.character(0:(ll-2)))
print(levels(blobSize))

blobSize <- as.numeric(as.character(blobSize));

points(ndatq$shootlong,ndatq$shootlat,cex=(blobSize+0.5)*scaling.factor,pch=21,col='black',bg='yellow')

legend("topright",
legend=round(seq(0,max(ndatq[,what.cpue]),length=7)*scaling.factor,2),
pch=rep(21,7),
pt.cex=seq(min(blobSize)+0.5,max(blobSize),length=7)*scaling.factor,
pt.bg=c(rep('yellow',7)),

bg='white',

x.intersp=1.5,xjust=0.5,col='black',horiz=F,cex=.6,title=what.cpue)


}

