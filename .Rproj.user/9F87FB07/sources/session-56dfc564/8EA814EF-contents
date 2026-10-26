library(librarian)
library(tidyverse)
library(ggplot2)
shelf(moments)
#question 1 

qnorm(0.975) #5% percentage point
pnorm(0.975)

qnorm(0.95) # 10% percentage point
qnorm(0.995) # 1% percentage point


qt(0.975, 20) #5% percentage point for t distribution with 20 degrees of freedom
qt(0.975, 5) #5% percentage point for t distribution with 5 degrees of freedeom
qt(0.975, 500) # 5% percentage point for t distribution with 500 degrees of freedom
#the bigger the sample size = degrees for freedom the closer it becomes to the
# value of a normal distribution

#question 2
#a) 
#b)
qt(0.975, 30)

#Question 4
fram <- read.csv("Fram_1d.csv")
head(fram)
mean(fram$sysbp)
sd(fram$sysbp)

#assign variables
mean_sysbp <- mean(fram$sysbp)
crit_value <- qnorm(0.975)
sample_size <- nrow(fram)
se_sysbp <- sd(fram$sysbp) / sqrt(sample_size)

#calculate
mean_sysbp - crit_value * se_sysbp
mean_sysbp + crit_value * se_sysbp

hist(fram$sysbp, breaks = 100)

#Question 5
heights <- read.csv("heights_1d.csv")
dim(heights)
head(heights)
nrow(heights)
mean_heights <- mean(heights$height)
sample_size2 <- nrow(heights)
se_heights <- sd(heights$height) / sqrt(sample_size2)
crit_value2 <- qt(0.975, 111)

# CI intervall
ci_heights_up <- mean_heights + crit_value2 * se_heights
ci_heights_low <- mean_heights - crit_value2 * se_heights
print(ci_heights_low)
print(ci_heights_up)

#Question 6
tri <- read.csv("triglyceride_1d.csv")
head(tri)
dim(tri)
hist(tri$trig, breaks = 50)
skewness(tri$trig)
kurtosis(tri$trig)
tri$logtrig = log(tri$trig)
head(tri)
hist(tri$logtrig, breaks = 50)
skewness(tri$logtrig)
kurtosis(tri$logtrig)

mean(tri$logtrig)
crit_value <- qnorm(0.975)
sd_tri <- sd(tri$logtrig)
sample_size_tri <- nrow(tri)
se_tri <- sd_tri / sqrt(sample_size_tri)

#Ci
ci_tri_up <- mean(tri$logtrig) + crit_value * se_tri
ci_tri_low <- mean(tri$logtrig) - crit_value * se_tri
print(ci_tri_up)
print(ci_tri_low)
exp(ci_tri_up)
exp(ci_tri_low)
