

tidyLfData <- function(input = lfs)   {

data(tsn)  # itis database of all species in the world

input$scientific.name <- as.character(tsn$completename[match(input$speccode,tsn$tsn)])   # match scientific names using tsn

input <- input[!duplicated(input),]                  #  remove any duplicates
input <- input[input$speccode != -9,]                #  remove missing species codes
input <- input[input$lngtclass != -9,]               #  remove missing length classes
input <- input[!is.na(input$lngtclass),]             #  remove missing length classes
input <- input[!is.na(input$lngtcode),]              #  remove missing length codes
input <- input[!is.na(input$scientific.name),]       # remove any missing scientific names
input$hlnoatlngt <- input$hlnoatlngt*input$subfact   # Multiply by the subfactor


## Standardise length codes to cms (some are in mms) and create bins of 0.5.


input$lngtclass[input$lngtcode == "."] <- input$lngtclass[input$lngtcode == '.']/10
input$lngtclass[input$lngtcode == "0"] <- input$lngtclass[input$lngtcode == "0"]/10


input$lngtclass[input$lngtcode != "5"] <- round(input$lngtclass[input$lngtcode != "5"])
input$lngtclass[input$lngtcode != "5"] <- input$lngtclass[input$lngtcode != "5"]+0.5


## Estimate weight of each fish.

# Note: It can be useful to quote results in terms of weight rather than numbers.
# This is usually done with parameters (a & b) from non-linear equations where
# Weight in grammes = aL^b  where L = length.

data(length.weight)   # attach a list of parameters courtesy of IMARES and Marine Science Scotland.

sv <- data.frame(scientific.name = 'Solea vulgaris',a=0.031696,b=2.603)
length.weight <- rbind(length.weight,sv)

# Match a and b parameters onto the correct species

input$a <- length.weight$a[match(input$scientific.name,length.weight$scientific.name)]
input$b <- length.weight$b[match(input$scientific.name,length.weight$scientific.name)]

input$hlwtatlngt<-((input$a*input$lngtclass^input$b)*input$hlnoatlngt) /1000    # calculate weight in kilos

input

}