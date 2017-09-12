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
young<-30
data <- read.table("Example5.csv", header=TRUE, sep=",")

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

young_age_female_average <- young_age_female_sum/young_female_count
young_age_male_average <- young_age_male_sum/young_male_count

old_age_female_average <- old_age_female_sum/old_female_count
old_age_male_average <- old_age_male_sum/old_male_count


young_bp_female_average <- young_bp_female_sum/young_female_count
young_bp_male_average <- young_bp_male_sum/young_male_count

young_cost_female_average <- young_cost_female_sum/young_female_count
young_cost_male_average <- young_cost_male_sum/young_male_count

young_cost_year_female_average <- young_cost_year_female_sum/young_female_count
young_cost_year_male_average <- young_cost_year_male_sum/young_male_count


old_bp_female_average <- old_bp_female_sum/old_female_count
old_bp_male_average <- old_bp_male_sum/old_male_count

old_cost_female_average <- old_cost_female_sum/old_female_count
old_cost_male_average <- old_cost_male_sum/old_male_count

old_cost_year_female_average <- old_cost_year_female_sum/old_female_count
old_cost_year_male_average <- old_cost_year_male_sum/old_male_count

colors = c(  "yellow", "red", "dark red", "blue1", "blue2", "green1", "blue3", "purple")

par(mar = c(9,4,4,1))
# Lines reordered alternating F,M groupd by Healthy, Sick, Dead, Age
plot(year, young_health_female_count, type='l', col=colors[1],lwd=2, main='Example 5 Young', ylab='Individuals', xlab = 'Time', ylim=c(0,35))
lines(year, young_health_male_count, type='l', col=colors[2], lwd=2)
lines(year, young_sick_female_count, type='l', col=colors[3],lwd=2)
lines(year, young_sick_male_count, type='l', col=colors[4],lwd=2)
lines(year, young_dead_female_count, type='l', col=colors[5],lwd=2)
lines(year, young_dead_male_count, type='l', col=colors[6],lwd=2)
lines(year, young_age_female_average, type='l', col=colors[7],lwd=2)
lines(year, young_age_male_average, type='l', col=colors[8],lwd=2)
legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M', 'Avg Age F',   'Avg Age M'), col=colors,inset=c(0,-0.9), lwd=2,ncol=4,xpd = TRUE,lty=1)


par(mar = c(9,4,4,1))
# Lines reordered alternating F,M groupd by BP, Cost, Costyear
plot(year, young_bp_female_average, type='l', col=colors[1], main='Example 5 Young', ylab='Individuals', xlab = 'Time', ylim=c(0,200),lwd=2)
lines(year, young_bp_male_average, type='l', col=colors[2], lwd=2)
lines(year, young_cost_female_average, type='l', col=colors[3],lwd=2)
lines(year, young_cost_male_average, type='l', col=colors[4],lwd=2)
lines(year, young_cost_year_female_average, type='l', col=colors[5],lwd=2)
lines(year, young_cost_year_male_average, type='l', col=colors[6],lwd=2)
legend('bottom', legend=c( 'Avg BP F', 'Avg BP M','Avg Cost F', 'Avg Cost M','Avg CostThisYear F','Avg CostThisYear M'), col=colors,inset=c(0,-0.9), lwd=2,ncol=3,xpd = TRUE,lty=1)


par(mar = c(9,4,4,1))
# Lines reordered alternating F,M groupd by Healthy, Sick, Dead, Age
plot(year, old_health_female_count, type='l', col=colors[1],main='Example 5 Old', ylab='Individuals', xlab = 'Time', ylim=c(0,50), lwd=2)
lines(year, old_health_male_count, type='l', col=colors[2], lwd=2)
lines(year, old_sick_female_count, type='l', col=colors[3],lwd=2)
lines(year, old_sick_male_count, type='l', col=colors[4],lwd=2)
lines(year, old_dead_female_count, type='l', col=colors[5],lwd=2)
lines(year, old_dead_male_count, type='l', col=colors[6],lwd=2)
lines(year, old_age_female_average, type='l', col=colors[7],lwd=2)
lines(year, old_age_male_average, type='l', col=colors[8],lwd=2)
legend('bottom', legend=c( 'Healthy F', 'Healthy M','Sick F', 'Sick M','Dead F','Dead M', 'Avg Age F',   'Avg Age M'), col=colors,inset=c(0,-0.9), lwd=2,ncol=4,xpd = TRUE,lty=1)


par(mar = c(9,4,4,1))
plot(year, old_bp_female_average, type='l', col=colors[1],main='Example 5 Old', ylab='Individuals', xlab = 'Time', ylim=c(0,500), lwd=2)
lines(year, old_bp_male_average, type='l', col=colors[2], lwd=2)
lines(year, old_cost_female_average, type='l', col=colors[3],lwd=2)
lines(year, old_cost_male_average, type='l', col=colors[4],lwd=2)
lines(year, old_cost_year_female_average, type='l', col=colors[5],lwd=2)
lines(year, old_cost_year_male_average, type='l', col=colors[6],lwd=2)
legend('bottom', legend=c( 'Avg BP F', 'Avg BP M','Avg Cost F', 'Avg Cost M','Avg CostThisYear F','Avg CostThisYear M'), col=colors,inset=c(0,-0.9), lwd=2,ncol=3,xpd = TRUE,lty=1)


