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

source("util.R")

run_example2 <- function(runs) {
  year=0:10
  
  health_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  dead_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  sick_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  for (run in 1:runs) 
  {
    
    filepath = file.path("Example2", paste("run-", run, ".csv",sep=""))
    data <- read.table(filepath, header=TRUE, sep=",")
    
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
    
    dead_count <- cumsum(dead_count)
    dead_df[nrow(dead_df)+1,] = c(dead_count)
    sick_df[nrow(sick_df)+1,] = c(sick_count)
    health_df[nrow(health_df)+1,] = c(health_count)
  }

  health_mean = colMeans(health_df)
  dead_mean = colMeans(dead_df)
  sick_mean = colMeans(sick_df)
  
  colors = c("red",  "blue1",   "green1")
  
  par(mar = c(9,4,4,1))

  save_csv(health_df,paste("results/Example2_n=", runs, "_health.csv", sep=''))
  save_csv(dead_df, paste("results/Example2_n=", runs, "_dead.csv", sep=''))
  save_csv(sick_df, paste("results/Example2_n=", runs, "_sick.csv", sep=''))
  
  plot(year, health_mean, type='l', col=colors[1], main=paste('Example 2 -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,100),lwd=2)
  lines(year, sick_mean, type='l', col=colors[2],lwd=2)
  lines(year, dead_mean, type='l', col=colors[3],lwd=2)
  legend('bottom', c('Healthy', 'Sick', 'Dead'), col=colors,lty=1,lwd=2, ncol=3,inset=c(0,-0.8),xpd = TRUE)
  
}