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
run_example5 <- function(runs) {
  year=0:10
  young<-30
  
  ##
  young_health_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_sick_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_dead_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_age_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_health_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_sick_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_dead_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_age_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  ##
  young_bp_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_bp_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_cost_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_cost_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_cost_year_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  young_cost_year_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  ##
  old_health_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_sick_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_dead_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_age_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_health_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_sick_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_dead_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_age_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  
  ##
  old_bp_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_bp_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_cost_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_cost_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_cost_year_male_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  old_cost_year_female_df <- data.frame(matrix(0, nrow = 0, ncol = 11))
  ##
  
  for (run in 1:runs) {
    
    
    filepath = file.path("Example5", paste("run-", run, ".csv",sep=""))
    data <- read.table(filepath, header=TRUE, sep=",")
    
    ##
    young_health_female_count <- rep(0, 11)
    young_sick_female_count <- rep(0, 11)
    young_dead_female_count <- rep(0,11)
    young_age_female_sum <- rep(0,11)
    young_health_male_count <- rep(0, 11)
    young_sick_male_count <- rep(0, 11)
    young_dead_male_count <- rep(0, 11)
    young_age_male_sum <- rep(0, 11)
    
    ##
    young_bp_male_sum <- rep(0, 11)
    young_bp_female_sum <- rep(0, 11)
    young_cost_male_sum <- rep(0, 11)
    young_cost_female_sum <- rep(0, 11)
    young_cost_year_male_sum <- rep(0, 11)
    young_cost_year_female_sum <- rep(0, 11)
    
    ##
    old_health_female_count <- rep(0, 11)
    old_sick_female_count <- rep(0, 11)
    old_dead_female_count <- rep(0,11)
    old_age_female_sum <- rep(0,11)
    old_health_male_count <- rep(0, 11)
    old_sick_male_count <- rep(0, 11)
    old_dead_male_count <- rep(0, 11)
    old_age_male_sum <- rep(0, 11)
    
    ##
    old_bp_male_sum <- rep(0, 11)
    old_bp_female_sum <- rep(0, 11)
    old_cost_male_sum <- rep(0, 11)
    old_cost_female_sum <- rep(0, 11)
    old_cost_year_male_sum <- rep(0, 11)
    old_cost_year_female_sum <- rep(0, 11)
    ##
    
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
        age <-  data[interesting_inst_male,paste("Age",i,sep="_")]
        bp <-  data[interesting_inst_male,paste("BP",i,sep="_")]
        cost <-  data[interesting_inst_male,paste("Cost",i,sep="_")]
        cost_year <-  data[interesting_inst_male,paste("CostThisYear",i,sep="_")]
        
        young_health_male_count[time+1] = young_health_male_count[time+1] + health*(age<=young)
        young_dead_male_count[time+1] = young_dead_male_count[time+1] + dead*(age<=young)
        young_sick_male_count[time+1] = young_sick_male_count[time+1] + sick*(age<=young)
        young_age_male_sum[time+1] = young_age_male_sum[time+1] + age*(health[time+1]+sick[time+1])*(age<=young)
        
        young_bp_male_sum[time+1] = young_bp_male_sum[time+1]+bp*(health[time+1]+sick[time+1])*(age<=young)
        young_cost_male_sum[time+1] = young_cost_male_sum[time+1]+cost*(health[time+1]+sick[time+1])*(age<=young)
        young_cost_year_male_sum[time+1] = young_cost_year_male_sum[time+1]+cost_year*(health[time+1]+sick[time+1])*(age<=young)
        
        old_health_male_count[time+1] = old_health_male_count[time+1] + health*(age>young)
        old_dead_male_count[time+1] = old_dead_male_count[time+1] + dead*(age>young)
        old_sick_male_count[time+1] = old_sick_male_count[time+1] + sick*(age>young)
        old_age_male_sum[time+1] = old_age_male_sum[time+1] + age*(health[time+1]+sick[time+1])*(age>young)
        
        old_bp_male_sum[time+1] = old_bp_male_sum[time+1]+bp*(health[time+1]+sick[time+1])*(age>young)
        old_cost_male_sum[time+1] = old_cost_male_sum[time+1]+cost*(health[time+1]+sick[time+1])*(age>young)
        old_cost_year_male_sum[time+1] = old_cost_year_male_sum[time+1]+cost_year*(health[time+1]+sick[time+1])*(age>young)
        
      }
      else if(length(interesting_inst_female) > 0)
      {
        dead <- data[interesting_inst_female,paste("Dead",i,sep="_")]
        health <- data[interesting_inst_female,paste("Healthy",i,sep="_")]
        sick  <- data[interesting_inst_female,paste("Sick",i,sep="_")]
        time <-  data[interesting_inst_female,paste("Time",i,sep="_")]
        age <-  data[interesting_inst_female,paste("Age",i,sep="_")]
        bp <-  data[interesting_inst_female,paste("BP",i,sep="_")]
        cost <-  data[interesting_inst_female,paste("Cost",i,sep="_")]
        cost_year <-  data[interesting_inst_female,paste("CostThisYear",i,sep="_")]
        
        young_health_female_count[time+1] = young_health_female_count[time+1] + health*(age<=young)
        young_dead_female_count[time+1] =young_dead_female_count[time+1] + dead*(age<=young)
        young_sick_female_count[time+1] = young_sick_female_count[time+1] + sick*(age<=young)
        young_age_female_sum[time+1] = young_age_female_sum[time+1] + age*(health[time+1]+sick[time+1])*(age<=young)
        
        young_bp_female_sum[time+1] = young_bp_female_sum[time+1]+bp*(health[time+1]+sick[time+1])*(age<=young)
        young_cost_female_sum[time+1] = young_cost_female_sum[time+1]+cost*(health[time+1]+sick[time+1])*(age<=young)
        young_cost_year_female_sum[time+1] = young_cost_year_female_sum[time+1]+cost_year*(health[time+1]+sick[time+1])*(age<=young)
        
        old_health_female_count[time+1] = old_health_female_count[time+1] + health*(age>young)
        old_dead_female_count[time+1] = old_dead_female_count[time+1] + dead*(age>young)
        old_sick_female_count[time+1] = old_sick_female_count[time+1] + sick*(age>young)
        old_age_female_sum[time+1] = old_age_female_sum[time+1] + age*(health[time+1]+sick[time+1])*(age>young)
      
        old_bp_female_sum[time+1] = old_bp_female_sum[time+1]+bp*(health[time+1]+sick[time+1])*(age>young)
        old_cost_female_sum[time+1] = old_cost_female_sum[time+1]+cost*(health[time+1]+sick[time+1])*(age>young)
        old_cost_year_female_sum[time+1] = old_cost_year_female_sum[time+1]+cost_year*(health[time+1]+sick[time+1])*(age>young)
        
        }
      
    }
    
    
    old_male_count = old_sick_male_count+old_health_male_count
    old_female_count=old_sick_female_count+old_health_female_count
    young_male_count = young_sick_male_count+young_health_male_count
    young_female_count=young_sick_female_count+young_health_female_count
    
    young_dead_male_count <- cumsum(young_dead_male_count)
    young_dead_female_count <- cumsum(young_dead_female_count)
    old_dead_male_count <- cumsum(old_dead_male_count)
    old_dead_female_count <- cumsum(old_dead_female_count)

    young_age_female_average <- young_age_female_sum/(young_female_count+1e-9)
    young_age_male_average <- young_age_male_sum/(young_male_count+1e-9)
    old_age_female_average <- old_age_female_sum/(old_female_count+1e-9)
    old_age_male_average <- old_age_male_sum/(old_male_count+1e-9)
    
    young_bp_female_average <- young_bp_female_sum/(young_female_count+1e-9)
    young_cost_female_average <- young_cost_female_sum/(young_female_count+1e-9)
    
    young_bp_male_average <- young_bp_male_sum/(young_male_count+1e-9)
    young_cost_male_average <- young_cost_male_sum/(young_male_count+1e-9)

    young_cost_year_female_average <- young_cost_year_female_sum/(young_female_count+1e-9)
    old_bp_female_average <- old_bp_female_sum/(old_female_count+1e-9)
    
    young_cost_year_male_average <- young_cost_year_male_sum/(young_male_count+1e-9)
    old_bp_male_average <- old_bp_male_sum/(old_male_count+1e-9)
    
    old_cost_female_average <- old_cost_female_sum/(old_female_count+1e-9)
    old_cost_year_female_average <- old_cost_year_female_sum/(old_female_count+1e-9)
    
    old_cost_male_average <- old_cost_male_sum/(old_male_count+1e-9)
    old_cost_year_male_average <- old_cost_year_male_sum/(old_male_count+1e-9)
    
    old_health_female_df[nrow(old_health_female_df)+1,] = c(old_health_female_count)
    old_dead_female_df[nrow(old_dead_female_df)+1,] = c(old_dead_female_count)
    old_sick_female_df[nrow(old_sick_female_df)+1,] = c(old_sick_female_count)
    old_age_female_df[nrow(old_age_female_df)+1,] = c(old_age_female_average)
    old_health_male_df[nrow(old_health_male_df)+1,] = c(old_health_male_count)
    old_dead_male_df[nrow(old_dead_male_df)+1,] = c(old_dead_male_count)
    old_sick_male_df[nrow(old_sick_male_df)+1,] = c(old_sick_male_count)
    old_age_male_df[nrow(old_age_male_df)+1,] = c(old_age_male_average)
    
    old_bp_female_df[nrow(old_bp_female_df)+1,] = c(old_bp_female_average)
    old_bp_male_df[nrow(old_bp_male_df)+1,] = c(old_bp_male_average)
    old_cost_female_df[nrow(old_cost_female_df)+1,] = c(old_cost_female_average)
    old_cost_male_df[nrow(old_cost_male_df)+1,] = c(old_cost_male_average)
    old_cost_year_female_df[nrow(old_cost_year_female_df)+1,] = c(old_cost_year_female_average)
    old_cost_year_male_df[nrow(old_cost_year_male_df)+1,] = c(old_cost_year_male_average)
    
    
    young_health_male_df[nrow(young_health_male_df)+1,] = c(young_health_male_count)
    young_sick_male_df[nrow(young_sick_male_df)+1,] = c(young_sick_male_count)
    young_dead_male_df[nrow(young_dead_male_df)+1,] = c(young_dead_male_count)
    young_age_male_df[nrow(young_age_male_df)+1,] = c(young_age_male_average)
    
    young_health_female_df[nrow(young_health_female_df)+1,] = c(young_health_female_count)
    young_sick_female_df[nrow(young_sick_female_df)+1,] = c(young_sick_female_count)
    young_dead_female_df[nrow(young_dead_female_df)+1,] = c(young_dead_female_count)
    young_age_female_df[nrow(young_age_female_df)+1,] = c(young_age_female_average)
    
    young_bp_female_df[nrow(young_bp_female_df)+1,] = c(young_bp_female_average)
    young_bp_male_df[nrow(young_bp_male_df)+1,] = c(young_bp_male_average)
    young_cost_female_df[nrow(young_cost_female_df)+1,] = c(young_cost_female_average)
    young_cost_male_df[nrow(young_cost_male_df)+1,] = c(young_cost_male_average)
    young_cost_year_female_df[nrow(young_cost_year_female_df)+1,] = c(young_cost_year_female_average)
    young_cost_year_male_df[nrow(young_cost_year_male_df)+1,] = c(young_cost_year_male_average)
    
  }
  
  young_health_female_mean <- colMeans(young_health_female_df)
  young_sick_female_mean <- colMeans(young_sick_female_df)
  young_dead_female_mean <- colMeans(young_dead_female_df)
  young_age_female_mean <- colMeans(young_age_female_df)
  young_health_male_mean <- colMeans(young_health_male_df)
  young_sick_male_mean <- colMeans(young_sick_male_df)
  young_dead_male_mean <- colMeans(young_dead_male_df)
  young_age_male_mean <- colMeans(young_age_male_df)
  
  ##
  young_bp_male_mean <- colMeans(young_bp_male_df)
  young_bp_female_mean <- colMeans(young_bp_female_df)
  young_cost_male_mean <- colMeans(young_cost_male_df)
  young_cost_female_mean <- colMeans(young_cost_female_df)
  young_cost_year_male_mean <- colMeans(young_cost_year_male_df)
  young_cost_year_female_mean <- colMeans(young_cost_year_female_df)
  
  ##
  old_health_female_mean <- colMeans(old_health_female_df)
  old_sick_female_mean <- colMeans(old_sick_female_df)
  old_dead_female_mean <- colMeans(old_dead_female_df)
  old_age_female_mean <- colMeans(old_age_female_df)
  old_health_male_mean <- colMeans(old_health_male_df)
  old_sick_male_mean <- colMeans(old_sick_male_df)
  old_dead_male_mean <- colMeans(old_dead_male_df)
  old_age_male_mean <- colMeans(old_age_male_df)
  
  ##
  old_bp_male_mean <- colMeans(old_bp_male_df)
  old_bp_female_mean <- colMeans(old_bp_female_df)
  old_cost_male_mean <- colMeans(old_cost_male_df)
  old_cost_female_mean <- colMeans(old_cost_female_df)
  old_cost_year_male_mean <- colMeans(old_cost_year_male_df)
  old_cost_year_female_mean <- colMeans(old_cost_year_female_df)
  
  save_csv(young_health_female_df, paste("results/Example5_n=", runs, "_young_health_female.csv", sep=''))
  save_csv(young_sick_female_df, paste("results/Example5_n=", runs, "_young_sick_female.csv", sep=''))
  save_csv(young_dead_female_df, paste("results/Example5_n=", runs, "_young_dead_female.csv", sep=''))
  save_csv(young_age_female_df, paste("results/Example5_n=", runs, "_young_age_female.csv", sep=''))
  
  save_csv(young_health_male_df, paste("results/Example5_n=", runs, "_young_health_male.csv", sep=''))
  save_csv(young_sick_male_df, paste("results/Example5_n=", runs, "_young_sick_male.csv", sep=''))
  save_csv(young_dead_male_df, paste("results/Example5_n=", runs, "_young_dead_male.csv", sep=''))
  save_csv(young_age_male_df, paste("results/Example5_n=", runs, "_young_age_male.csv", sep=''))
  
  save_csv(young_bp_female_df, paste("results/Example5_n=", runs, "_young_bp_female.csv", sep=''))
  save_csv(young_cost_female_df, paste("results/Example5_n=", runs, "_young_cost_female.csv", sep=''))
  save_csv(young_cost_year_female_df, paste("results/Example5_n=", runs, "_young_cost_year_female.csv", sep=''))
  
  save_csv(young_bp_male_df, paste("results/Example5_n=", runs, "_young_bp_male.csv", sep=''))
  save_csv(young_cost_male_df, paste("results/Example5_n=", runs, "_young_cost_male.csv", sep=''))
  save_csv(young_cost_year_male_df, paste("results/Example5_n=", runs, "_young_cost_year_male.csv", sep=''))
  
  
  save_csv(old_health_female_df, paste("results/Example5_n=", runs, "_old_health_female.csv", sep=''))
  save_csv(old_sick_female_df, paste("results/Example5_n=", runs, "_old_sick_female.csv", sep=''))
  save_csv(old_dead_female_df, paste("results/Example5_n=", runs, "_old_dead_female.csv", sep=''))
  save_csv(old_age_female_df, paste("results/Example5_n=", runs, "_old_age_female.csv", sep=''))
  
  save_csv(old_health_male_df, paste("results/Example5_n=", runs, "_old_health_male.csv", sep=''))
  save_csv(old_sick_male_df, paste("results/Example5_n=", runs, "_old_sick_male.csv", sep=''))
  save_csv(old_dead_male_df, paste("results/Example5_n=", runs, "_old_dead_male.csv", sep=''))
  save_csv(old_age_male_df, paste("results/Example5_n=", runs, "_old_age_male.csv", sep=''))
  
  save_csv(old_bp_female_df, paste("results/Example5_n=", runs, "_old_bp_female.csv", sep=''))
  save_csv(old_cost_female_df, paste("results/Example5_n=", runs, "_old_cost_female.csv", sep=''))
  save_csv(old_cost_year_female_df, paste("results/Example5_n=", runs, "_old_cost_year_female.csv", sep=''))
  
  save_csv(old_bp_male_df, paste("results/Example5_n=", runs, "_old_bp_male.csv", sep=''))
  save_csv(old_cost_male_df, paste("results/Example5_n=", runs, "_old_cost_male.csv", sep=''))
  save_csv(old_cost_year_male_df, paste("results/Example5_n=", runs, "_old_cost_year_male.csv", sep=''))
  
  colors = c(  "yellow", "red", "dark red", "blue1", "blue2", "green1", "blue3", "purple")
  
  par(mar = c(9,4,4,1))
  plot(year, young_health_female_mean, type='l', col=colors[1],lwd=2, main=paste('Example 5 Young -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,35))
  lines(year, young_health_male_mean, type='l', col=colors[2], lwd=2)
  lines(year, young_sick_female_mean, type='l', col=colors[3],lwd=2)
  lines(year, young_sick_male_mean, type='l', col=colors[4],lwd=2)
  lines(year, young_dead_female_mean, type='l', col=colors[5],lwd=2)
  lines(year, young_dead_male_mean, type='l', col=colors[6],lwd=2)
  lines(year, young_age_female_mean, type='l', col=colors[7],lwd=2)
  lines(year, young_age_male_mean, type='l', col=colors[8],lwd=2)
  legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M', 'Avg Age F',   'Avg Age M'), col=colors, inset=c(0,-0.8), lwd=2,ncol=4,xpd = TRUE,lty=1)
  
  par(mar = c(9,4,4,1))
  plot(year, young_bp_female_mean, type='l', col=colors[1], main=paste('Example 5 Young -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,200),lwd=2)
  lines(year, young_bp_male_mean, type='l', col=colors[2], lwd=2)
  lines(year, young_cost_female_mean, type='l', col=colors[3],lwd=2)
  lines(year, young_cost_male_mean, type='l', col=colors[4],lwd=2)
  lines(year, young_cost_year_female_mean, type='l', col=colors[5],lwd=2)
  lines(year, young_cost_year_male_mean, type='l', col=colors[6],lwd=2)
  legend('bottom', legend=c( 'Avg BP F', 'Avg BP M','Avg Cost F', 'Avg Cost M','Avg CostThisYear F','Avg CostThisYear M'), col=colors,inset=c(0,-0.8), lwd=2,ncol=3,xpd = TRUE,lty=1)
  
  par(mar = c(9,4,4,1))
  plot(year, old_health_female_mean, type='l', col=colors[1],main=paste('Example Old -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,50), lwd=2)
  lines(year, old_health_male_mean, type='l', col=colors[2], lwd=2)
  lines(year, old_sick_female_mean, type='l', col=colors[3],lwd=2)
  lines(year, old_sick_male_mean, type='l', col=colors[4],lwd=2)
  lines(year, old_dead_female_mean, type='l', col=colors[5],lwd=2)
  lines(year, old_dead_male_mean, type='l', col=colors[6],lwd=2)
  lines(year, old_age_female_mean, type='l', col=colors[7],lwd=2)
  lines(year, old_age_male_mean, type='l', col=colors[8],lwd=2)
  legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M', 'Avg Age F',   'Avg Age M'), col=colors, lwd=2,ncol=4,inset=c(0,-0.8),xpd = TRUE,lty=1)

  par(mar = c(9,4,4,1))
  plot(year, old_bp_female_mean, type='l', col=colors[1],main=paste('Example 5 Old -', runs, 'run(s).'), ylab='Individuals', xlab = 'Time', ylim=c(0,500), lwd=2)
  lines(year, old_bp_male_mean, type='l', col=colors[2], lwd=2)
  lines(year, old_cost_female_mean, type='l', col=colors[3],lwd=2)
  lines(year, old_cost_male_mean, type='l', col=colors[4],lwd=2)
  lines(year, old_cost_year_female_mean, type='l', col=colors[5],lwd=2)
  lines(year, old_cost_year_male_mean, type='l', col=colors[6],lwd=2)
  legend('bottom', legend=c( 'Avg BP F', 'Avg BP M','Avg Cost F', 'Avg Cost M','Avg CostThisYear F','Avg CostThisYear M'), col=colors, lwd=2,ncol=3,inset=c(0,-0.8),xpd = TRUE,lty=1)

  
 
}
