
ChronsTrack2SpatialLine <- function(input=nchrons,which.tow=1)

{
#input reorganised chrons data

Stn      <- input[input$id == which.tow,]        #Select station
coordinates(Stn) <- ~long+lat                    #add on coordinates
StnL     <- Lines(list(Line(Stn)),ID=as.character(which.tow))
return(StnL)
}
