

tidyChronData <- function(input = chrons)

{
input <- input[input$haulval=='V',]          # only valid hauls
input <- input[!duplicated(input),]          # chuck any duplicates
input
}
