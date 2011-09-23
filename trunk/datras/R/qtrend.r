qtrend.f <- function(year,quarter,start.year=1958){
#Function to calculate 'trend' vector for time-series:
nyear=year-start.year
trend=quarter+nyear*4
trend
}

         
         
         