
#     Merge lfs with the chrons. One reason is that only the +ve part of the lf data are typically stored
#     and you need a complete list of stations to find the zero hauls.
#     You also need the extra information in the chron file, ie. latitude and longitude.

mergeChronsLfs <- function(chrons=chrons,length.frequencies=spurdog) {

#Input are the raw chrons and the lfs by species

short.chrons <- data.frame(quarter=chrons$quarter,country=chrons$country,ship=chrons$ship,
gear=chrons$gear,stno=chrons$stno,haulno=chrons$haulno,
shootlat=chrons$hauldur,daynight=chrons$daynight,shootlat=chrons$shootlat,shootlong=chrons$shootlong,haullat=chrons$haullat,
haullong=chrons$haullong,statrec=chrons$statrec,depth=chrons$depth,haulval=chrons$haulval,distance=chrons$distance,
datim.shoot=chrons$datim.shot,datim.haul=chrons$datim.haul)


short.length.frequencies <- length.frequencies[,-c(1,5,6,7)]

merged.lfs  <- merge(short.chrons,short.length.frequencies,all=T)

merged.lfs$scientific.name <- short.length.frequencies$scientific.name[1]
merged.lfs$hlnoatlngt <- 0

merged.lfs

}

