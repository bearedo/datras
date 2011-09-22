
plot.Lfs<- function(fish=merged.dab,start.year=1996,end.year=2010,what.quarter="3",chrons1=chrons)
{
hd<-tapply(chrons1$hauldur,list(chrons1$year,chrons1$quarter),sum,na.rm=T)/60  # Get the haul duration
what.quarter <- as.character(what.quarter)
fish.yr.l<-tapply(fish$hlnoatlngt,list(fish$year,fish$lngtclass,fish$quarter),sum,na.rm=T)  # sum numbers caught over year, length, and quarter
dd<-dim(fish.yr.l)
fish.yr.l.h<-fish.yr.l[,,what.quarter]/matrix(rep(hd[,what.quarter],dd[2]),ncol=dd[2])      # get cpue
for(i in start.year:end.year ){                                                             # loop over year
input <- fish.yr.l.h[as.character(i),]
if(length(input)>0){
barplot(input, ylim=c(0,max(fish.yr.l.h,na.rm=T)),space=0)
title(as.character(i))}
else{frame() }
}
}
