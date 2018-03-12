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

run_example1 <- function(runs) {
  year <- 0:10
  alive_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  dead_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  names(alive_df) <- paste0('year_', year)
  names(dead_df) <- paste0('year_', year)
  
  for (run in 1:runs) {
    
    filepath = file.path("Example1", paste("run-", run, ".csv",sep=""))
    data <- read.table(filepath, header=TRUE, sep=",")
    
    alive_count <- rep(0, 11)
    dead_count <- rep(0, 11)
    
    for(i in 0:99)
    {
      
      inst <- data[,paste("InstructionNumber",i,sep="_")]
      interesting_inst = which(inst == 0.1)
      
      dead <- data[interesting_inst,paste("Dead",i,sep="_")]
      alive <- data[interesting_inst,paste("Alive",i,sep="_")]
      time <-  data[interesting_inst,paste("Time",i,sep="_")]
      
      alive_count[time+1] = alive_count[time+1] + alive
      dead_count[time+1] = dead_count[time+1] + dead
    }
  
    
    alive_df[nrow(alive_df)+1,] = c(alive_count)
    dead_count <- cumsum(dead_count)
    dead_df[nrow(dead_df)+1,] = c(dead_count)
  }
  
  alive_mean = colMeans(alive_df)
  dead_mean = colMeans(dead_df)
  
  colors = c("red",  "blue1")
  
 
  save_csv(alive_df,paste("results/Example1_n=", runs, "_alive.csv", sep=''))
  save_csv(dead_df, paste("results/Example1_n=", runs, "_dead.csv", sep=''))
  par(mar = c(9,4,4,1))
  
  p <- plot(year, alive_mean, type='l', col=colors[1], main=paste('Example 1 -', runs, 'run(s).') , ylab='Individuals', xlab = 'Time', ylim=c(0,100),lwd=2)
  lines(year, dead_mean, type='l', col=colors[2],lwd=2)
  legend('bottom', c('Alive', 'Dead'), col=colors,lty=1,lwd=2, ncol=2,inset=c(0,-0.8),xpd = TRUE)
}