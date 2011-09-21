
### Code to produce tow tracks in spatial lines data.frame format

ChronsTrackToSpatialLineDF <- function(input=chrons,what.ship=c('SCO3','DAN2')) {

## The data must be organised so that the haul position follows the shoot position in a single column

input <- input[input$ship==what.ship,]

input <- orderBy(~ship+datim.shot+datim.haul,data=input)
input1  <-  data.frame(long=input$shootlong,lat=input$shootlat,datim.shot=input$datim.shot,hauldur=input$hauldur,gear=input$gear,ship=input$ship,country=input$country)
input2  <-  data.frame(long=input$haullong,lat=input$haullat,datim.shot=input$datim.haul,hauldur=input$hauldur,gear=input$gear,ship=input$ship,country=input$country)

ninput <- rbind(input1,input2)

ninput <- orderBy(~ship+gear+datim.shot,data=ninput)
dimnames(ninput)[[2]][3] <- 'datim'

lng <- length(input1[,1])
ninput$id <- as.character(rep(1:lng,rep(2,lng)))
spl <- as.list(1:length(input[,1]))
for(i in 1:length(input[,1])){
print(i)
spl[i] <- ChronsTrackToSpatialLine(input=ninput,which.tow=i)
 }


spl1 <- SpatialLines(spl)
details<- data.frame(id = as.character(1:length(input[,1])),gear=input$gear,country=input$country,ship=input$ship)
sp.df<-SpatialLinesDataFrame(sl=spl1,data=details)

proj4string(sp.df) <- CRSargs(CRS("+init=epsg:4326"))

sp.df

}
