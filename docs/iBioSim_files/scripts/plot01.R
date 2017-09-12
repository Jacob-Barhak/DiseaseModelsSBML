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