#*******************************************************************************
#
# Copyright (C) 2017 Jacob Barhak, Leandro Watanabe, Chris Myers 
# This file is part of the SBML Arrays Examples.
#
# This script is free software: you can redistribute it and/or 
# modify it under the terms of the GNU Lesser General Public License as 
# published by the Free Software Foundation, either version 3 of the 
# License, or (at your option) any later version.
#
# ADDITIONAL CLARIFICATION
# This script is distributed in the hope that it will be 
# useful, but "as is" and WITHOUT ANY WARRANTY of any kind, including any 
# warranty that it will not infringe on any property rights of another party 
# or the IMPLIED WARRANTIES OF MERCHANTABILITY or FITNESS FOR A PARTICULAR 
# PURPOSE. THE AUTHORS assume no responsibilities with respect to the use 
# of it.
#  
#*******************************************************************************

year=0:10
data <- read.table("Example2.csv", header=TRUE, sep=",")

health_count <- rep(0, 11)
sick_count <- rep(0, 11)
dead_count <- rep(0, 11)

for(i in 0:99)
{
  inst <- data[,paste("InstructionNumber",i,sep="_")]
  interesting_inst = which(inst == 0.1)
  
  dead <- data[interesting_inst,paste("Dead",i,sep="_")]
  health <- data[interesting_inst,paste("Healthy",i,sep="_")]
  sick  <- data[interesting_inst,paste("Sick",i,sep="_")]
  time <-  data[interesting_inst,paste("Time",i,sep="_")]

  health_count[time+1] = health_count[time+1] + health
  dead_count[time+1] = dead_count[time+1] + dead
  sick_count[time+1] = sick_count[time+1] + sick
}

par(mar = c(9,4,4,1))
dead_count <- cumsum(dead_count)
colors = c("red",  "blue1",   "green1")

plot(year, health_count, type='l', col=colors[1], main='Example 2', ylab='Individuals', xlab = 'Time', ylim=c(0,100),lwd=2)
lines(year, sick_count, type='l', col=colors[2],lwd=2)
lines(year, dead_count, type='l', col=colors[3],lwd=2)
legend('bottom', c('Healthy', 'Sick', 'Dead'), col=colors,lty=1,lwd=2, ncol=3,inset=c(0,-0.8), xpd = TRUE)
