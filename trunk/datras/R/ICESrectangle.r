ICESrectangle <- function(dF,nameLon="shootlong",nameLat="shootlat"){
                    rectChar1n2 <- as.integer( 2 * (dF[,nameLat]-35.5) )

                    rectChar3 <- ifelse(dF[,nameLon]<=-40,"A"
                                ,ifelse(dF[,nameLon]<=-30,"B"
                                ,ifelse(dF[,nameLon]<=-20,"C"
                                ,ifelse(dF[,nameLon]<=-10,"D"
                                ,ifelse(dF[,nameLon]<= 0 ,"E"
                                ,ifelse(dF[,nameLon]<=10,"F"
                                ,ifelse(dF[,nameLon]<=20,"G"
                                ,ifelse(dF[,nameLon]<=30,"H"
                                ,"I"))))))))

                    rectChar4 <- as.integer( dF[,nameLon] %% 10)

                    rectID <- paste(rectChar1n2,rectChar3,rectChar4,sep='')
                return(rectID)}

