                                 ## Create a dataset of numbers by age per ICES statistical rectangle


 NosatAgebyYearStatSquare <-function(lfdat = merged.haddock,alk = alk,chrons=chrons, 
 what.species ='Melanogrammus aeglefinus',plus.group=10)
 
 {
 
 #lfdat = merged.haddock;
 #lfdat <- merged.plaice;
 
 #what.species ='Pleuronectes platessa'
 #what.species = 'Melanogrammus aeglefinus'
 

 
  nalk <- alk[alk$scientific.name==what.species,]
  nalk$age <- ifelse(!is.na(nalk$age) & nalk$age >= plus.group, plus.group, nalk$age)    # Make a plus group
  
  min.l <- min(nalk$lngtclass,na.rm=T)                                           # Min and max
  max.l <- max(nalk$lngtclass,na.rm=T)
  
  lfdat <- lfdat[is.na(lfdat$lngtclass) | lfdat$lngtclass > min.l & lfdat$lngtclass < max.l ,]  # Remove lengths that are too big and too small

  # Make the missing lngtclass zero for this process 
  
  lfdat$lngtclass <- ifelse(is.na(lfdat$lngtclass),0,lfdat$lngtclass)
   
  slfs <- aggregate(list(hlnoatlngt=lfdat$hlnoatlngt,hlwtatlngt=lfdat$hlwtatlngt),list(lngtclass=lfdat$lngtclass,
  year=lfdat$year,quarter=lfdat$quarter,
  statrec=lfdat$statrec),sum,na.rm=T)    #sum nos and weights over length, year, qtr, and statrectangle
  
  
  salks1 <- aggregate(list(f=nalk$age),list(lngtclass=nalk$lngtclass,age=nalk$age,year=nalk$year,quarter=nalk$quarter),length) # Frequencies of each age length category
  
   
  salks2 <- aggregate(list(total.aged=salks1$f),list(lngtclass=salks1$lngtclass,year=salks1$year,quarter=salks1$quarter),sum) # sum of the frequencies 
  
  salks3 <- merge(salks1,salks2,all=T) #merge the two alk tables
  
  salks3$p <- salks3$f/salks3$total.aged  #Get the proportion
  

  # Put the proportion and ages onto the length frequency data
  
  m1 <- paste(slfs$lngtclass,slfs$year,slfs$quarter)
  m2 <- paste(salks3$lngtclass,salks3$year,salks3$quarter)
  
  slfs$p <- salks3$p[match(m1,m2)]
  slfs$age <- salks3$age[match(m1,m2)]
  
     
  
  slfs$hlnoatage <- slfs$hlnoatlngt*slfs$p      # calculate the numbers at age
  slfs$hlwtatage <- slfs$hlwtatlngt*slfs$p      # calculate the weights at age
  
  slfs$hlnoatage <- ifelse(is.na(slfs$hlnoatage),0,slfs$hlnoatage)
  slfs$hlwtatage <- ifelse(is.na(slfs$hlwtatage),0,slfs$hlwtatage)
  
  slfs$age <- as.character(slfs$age)
  slfs$age <- ifelse(is.na(slfs$age),"missing",slfs$age)
  
  
  #Collapse to age
  
  slfs3 <- aggregate(list(hlnoatage=slfs$hlnoatage,hlwtatage=slfs$hlwtatage),
  list(year=slfs$year,quarter=slfs$quarter,age=slfs$age,statrec=slfs$statrec),sum,na.rm=T)
  
  slfs3$age <- ifelse(slfs3$age == 'missing', NA,slfs3$age)
  slfs3$age <- as.numeric(as.character(slfs3$age))
  
   
  #Get the hauldurations from the chrons 
  
  haulduration <- aggregate(list(haulduration=chrons$hauldur), list(year=chrons$year,statrec=chrons$statrec,quarter=chrons$quarter),sum,na.rm=T)
  haulduration$haulduration <- haulduration$haulduration/60
  
 
  out <- merge(slfs3,haulduration,all.x=T)    # merge the data with the hauldurations. Note: relevant at this level of aggregation.
  
   out <- orderBy(~year+age,data=out)          # order the data
   
   out$hlnoatage[is.na(out$hlnoatage)] <-0
   out$hlwtatage[is.na(out$hlwtatage)] <-0
   #out$age <- ifelse(is.na(out$age),'missing',out$age)
   
   out0 <- out[out$hlnoatage==0,]  # These are the stations where no fish at all were recorded.
   out0 <- out0[,-4]
   age.dist <- data.frame(age=c(min(out$age,na.rm=T):max(out$age,na.rm=T)),hlnoatage=0,hlwtatage=0 )
   out1 <- merge(out0,age.dist,all=T)
   
   
   out <- out[out$hlnoatage != 0,] # positive stations
   
   out <- rbind(out,out1)
   
   out }
   
  