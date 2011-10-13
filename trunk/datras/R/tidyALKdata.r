


tidyALKdata <- function(input = alk)

{

#Add on some stuff and tidy up

input$lngtclass[input$lngtcode == "."] <- input$lngtclass[input$lngtcode == '.']/10
input$lngtclass[input$lngtcode == 0] <- input$lngtclass[input$lngtcode == 0]/10

#Round down length classes & add 0.5


input$lngtclass[input$lngtcode != "5"] <- round(input$lngtclass[input$lngtcode != "5"])
input$lngtclass[input$lngtcode != "5"] <- input$lngtclass[input$lngtcode != "5"]+0.5


data(tsn)  # itis database of ALL species in the world
input$scientific.name <- as.character(tsn$completename[match(input$speccode,tsn$tsn)])   # match scientific name onto species code

input <- input[!duplicated(input),]     #  remove any duplicates
input <- input[input$speccode != -9,]   #  missing species codes
input <- input[input$lngtclass != -9,]  #  missing length classes
input <- input[input$age != -9,]        # missing ages
input <- input[!is.na(input$lngtcode),] #  missing length codes
input <- input[!is.na(input$scientific.name),] # missing species codes

input$age <- as.numeric(as.character(input$age))

input

}