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
run_example3 <- function(runs) {
  year=0:10
  
  health_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  dead_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  sick_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  health_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  dead_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  sick_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  for (run in 1:runs) {
    
    health_female_count <- rep(0, 11)
    sick_female_count <- rep(0, 11)
    dead_female_count <- rep(0,11)
    health_male_count <- rep(0, 11)
    sick_male_count <- rep(0, 11)
    dead_male_count <- rep(0, 11)
  
    filepath = file.path("Example3", paste("run-", run, ".csv",sep=""))
    data <- read.table(filepath, header=TRUE, sep=",")
    
    for(i in 0:99) {
      
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
        else if(length(interesting_inst_female) > 0)
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
    
    health_male_df[nrow(health_male_df)+1,] = c(health_male_count)
    dead_male_df[nrow(dead_male_df)+1,] = c(dead_male_count)
    sick_male_df[nrow(sick_male_df)+1,] = c(sick_male_count)
    
    health_female_df[nrow(health_female_df)+1,] = c(health_female_count)
    dead_female_df[nrow(dead_female_df)+1,] = c(dead_female_count)
    sick_female_df[nrow(sick_female_df)+1,] = c(sick_female_count)
  }
  
  
  health_female_mean = colMeans(health_female_df)
  dead_female_mean = colMeans(dead_female_df)
  sick_female_mean = colMeans(sick_female_df)
  
  
  health_male_mean = colMeans(health_male_df)
  dead_male_mean = colMeans(dead_male_df)
  sick_male_mean = colMeans(sick_male_df)
  
  
  save_csv(health_male_df, paste("results/Example3_n=", runs, "_health_male.csv", sep=''))
  save_csv(dead_male_df, paste("results/Example3_n=", runs, "_dead_male.csv", sep=''))
  save_csv(sick_male_df, paste("results/Example3_n=", runs, "_sick_male.csv", sep=''))
  save_csv(health_female_df, paste("results/Example3_n=", runs, "_health_female.csv", sep=''))
  save_csv(dead_female_df, paste("results/Example3_n=", runs, "_dead_female.csv", sep=''))
  save_csv(sick_female_df, paste("results/Example3_n=", runs, "_sick_female.csv", sep=''))
  
  colors = c(  "yellow", "red", "dark red", "blue1", "blue3", "green1")
  
  par(mar = c(9,4,4,1))
  plot(year, health_female_mean, type='l', col=colors[1], main=paste('Example 3 -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,50),lwd=2)
  lines(year, health_male_mean, type='l', col=colors[2], lwd=2)
  lines(year, sick_female_mean, type='l', col=colors[3],lwd=2)
  lines(year, sick_male_mean, type='l', col=colors[4],lwd=2)
  lines(year, dead_female_mean, type='l', col=colors[5],lwd=2)
  lines(year, dead_male_mean, type='l', col=colors[6],lwd=2)
  
  legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M'), col=colors, lwd=2,ncol=3, inset=c(0,-0.8),xpd=TRUE,lty=1)
  
  
}

