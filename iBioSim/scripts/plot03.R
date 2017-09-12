#*******************************************************************************
#  
# This file is part of iBioSim. Please visit <http://www.async.ece.utah.edu/ibiosim>
# for the latest version of iBioSim.
#
# Copyright (C) 2017 University of Utah
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the Apache License. A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online at <http://www.async.ece.utah.edu/ibiosim/License>.
#  
#*******************************************************************************

year=0:10
data <- read.table("Example3.csv", header=TRUE, sep=",")
health_female_count <- rep(0, 11)
sick_female_count <- rep(0, 11)
dead_female_count <- rep(0,11)
health_male_count <- rep(0, 11)
sick_male_count <- rep(0, 11)
dead_male_count <- rep(0, 11)

for(i in 0:99)
{
  inst <- data[,paste("InstructionNumber",i,sep="_")]
  sex <- data[,paste("Male",i,sep="_")]
  interesting_inst_male = which(inst == 0.1 & sex == 1)
  interesting_inst_female = which(inst == 0.1 & sex == 0)
  if(length(interesting_inst_male) > 0)
  {
  dead <- data[interesting_inst_male,paste("Dead",i,sep="_")]
  health <- data[interesting_inst_male,paste("Healthy",i,sep="_")]
  sick  <- data[interesting_inst_male,paste("Sick",i,sep="_")]
  time <-  data[interesting_inst_male,paste("Time",i,sep="_")]
  
  health_male_count[time+1] = health_male_count[time+1] + health
  dead_male_count[time+1] = dead_male_count[time+1] + dead
  sick_male_count[time+1] = sick_male_count[time+1] + sick
  }
  else   if(length(interesting_inst_female) > 0)
  {
  dead <- data[interesting_inst_female,paste("Dead",i,sep="_")]
  health <- data[interesting_inst_female,paste("Healthy",i,sep="_")]
  sick  <- data[interesting_inst_female,paste("Sick",i,sep="_")]
  time <-  data[interesting_inst_female,paste("Time",i,sep="_")]
  
  health_female_count[time+1] = health_female_count[time+1] + health
  dead_female_count[time+1] = dead_female_count[time+1] + dead
  sick_female_count[time+1] = sick_female_count[time+1] + sick
  }
}

dead_male_count <- cumsum(dead_male_count)
dead_female_count <- cumsum(dead_female_count)

#colors = c("red",  "blue3",   "green1", "yellow","dark red", "blue1")
colors = c(  "yellow", "red", "dark red", "blue1", "blue3", "green1")

par(mar = c(9,4,4,1))
# Lines reordered alternating F,M groupd by Healthy, Sick, Dead
plot(year, health_female_count, type='l', col=colors[1], main='Example 3', ylab='Individuals', xlab = 'Time', ylim=c(0,50),lwd=2)
lines(year, health_male_count, type='l', col=colors[2], lwd=2)
lines(year, sick_female_count, type='l', col=colors[3],lwd=2)
lines(year, sick_male_count, type='l', col=colors[4],lwd=2)
lines(year, dead_female_count, type='l', col=colors[5],lwd=2)
lines(year, dead_male_count, type='l', col=colors[6],lwd=2)

#legend('bottom', legend=c('Healthy M', 'Sick M', 'Dead M','Healthy F', 'Sick F', 'Dead F'), col=colors,inset=c(0,-0.9), lwd=2,ncol=2,xpd = TRUE,lty=1)
legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M'), col=colors,inset=c(0,-0.9), lwd=2,ncol=3,xpd = TRUE,lty=1)




