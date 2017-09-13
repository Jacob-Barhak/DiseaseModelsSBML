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

data <- read.table("Example1.csv", header=TRUE, sep=",")
data <- data[, order(names(data))]
dead <- subset(data, select=grep("Dead", names(data)))
time <- subset(data, select=grep("Time", names(data)))
index<-apply(dead,2,function(x){tail(which(x<1),1)})
alive_time <- time[matrix(index,1)]
year <- 0:9
alive_count <- lapply(year, function(x) sum(alive_time >= x))
dead_count <- lapply(alive_count, function(x) 100-x)

par(mar = c(9,4,4,1))
colors = c("red",  "blue1")
plot(year, alive_count, type='l', col=colors[1], main='Example 1', ylab='Individuals', xlab = 'Time', ylim=c(0,100),lwd=2)
lines(year, dead_count, type='l', col=colors[2],lwd=2)
legend('bottom', c('Alive', 'Dead'), col=colors,lty=1,lwd=2,inset=c(0,-0.8), ncol=2,xpd = TRUE)
