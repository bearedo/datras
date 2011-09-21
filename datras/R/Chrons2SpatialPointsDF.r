
### Create a SpatialPointsDataFrame from chrons (see package: sp)

Chrons2SpatialPointsDF <- function(data=chrons) {
coords  <- coordinates(chrons[c("shootlong","shootlat")])
#-Coords can be turned into spatial points and if you add a data.frame to it, to a SpatialPoints dataframe
sp      <- SpatialPoints(coords)
spDF    <- SpatialPointsDataFrame(coords,data=data.frame(year=chrons$year,month=chrons$month,day=chrons$day,ship=chrons$ship,timeshot=chrons$timeshot,
gear=chrons$gear,haulduration=chrons$hauldur))
#-Add a projection attribute
proj4string(spDF) <- CRSargs(CRS("+init=epsg:4326"))
spDF
}
