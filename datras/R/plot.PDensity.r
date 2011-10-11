
plot.PDensity<- function(fish=merged.plaice,what.quarter="3",chrons1=chrons,bw=0.8)
{

#fish <- merged.plaice
#what.quarter <- "3"
#chrons1 <- chrons

hd<-tapply(chrons1$hauldur,list(chrons1$year,chrons1$quarter),sum,na.rm=T)/60  # Get the haul duration
what.quarter <- as.character(what.quarter)
fish.yr.l<-tapply(fish$hlnoatlngt,list(fish$year,fish$lngtclass,fish$quarter),sum,na.rm=T)  # sum numbers caught over year, length, and quarter
dd<-dim(fish.yr.l)
fish.yr.l.h<-fish.yr.l[,,what.quarter]/matrix(rep(hd[,what.quarter],dd[2]),ncol=dd[2])      # get cpue

for(i in sort(unique(fdat$year)) ){   
print(i)                                                                                    # loop over year
input <- fish.yr.l.h[as.character(i),]
input1 <- input[!is.na(input)]
if(length(input1)>0){
d<- density(rep(as.numeric(names(input1)),round(input1)),bw=bw)
plot(d,type='n',main=as.character(i),xlim=c(1.3,50))
polygon(d,col='wheat')

}
else{frame() }
}
}


