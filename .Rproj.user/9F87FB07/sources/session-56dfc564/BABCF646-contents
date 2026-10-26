#read libraries
library(librarian)
library(tidyverse)
library(ggplot2)
shelf(BSDA)
#Questions
#2.1 Blood pressure
Data <- read.csv("Mphil.Data.csv")
t.test(x=Data$Sample001, mu = 100)
#Question a): 
#1. normally distributed, continuous outome
#2. independent outcomes
#3. simple random sample from population
#4. unknown variance of the population outcome 

#question 2 b): sample mean = 102.444, CI[98.81, 106.07],
# p value = 0.1589

#Qestion c): Given our sample, with a p value of 0.1589 
# there is no evidence of rejecting the Null hypothesis (which is that IQ mean is 100)

t.test(x = Data$Sample001, mu = 100, conf.level = 0.90)

#question d) the confidence intervals have become narrower, the p value has not!

#Question 2.2
?t.test
t.test( x = Data$Sample001, 
        mu = 95, 
        alternative = "less")
#a) p - value = 0.9993 --> I conclude: Given our sample there is no evidence to reject Null hypothesis

#b) 

#c) maybe when you are only interested if sth surpasses a theshold (e.g. CPR)

#Question 2.3
p.value.1 <- t.test( x = Data$Sample001, 
        mu = 101)$p.value
p.value.2 <- t.test( x = Data$Sample001, 
                  mu = 102)$p.value
p.value.3 <- t.test( x = Data$Sample001, 
                     mu = 103)$p.value
# 2.3 g)
sum.p.values <- p.value.1 + p.value.2 + p.value.3
sum.p.values
#the sum of p values does not tell us a meaningful conclusion

#Question 2.4 IQ
iq <- read.csv("IQ_one.csv")
t.test(x = iq, 
       mu = 66)
# p value = 0.03511
t.test(x = iq, 
       mu = 67)
# p value = 0.42
t.test(x = iq, 
       mu = 68)
# p value = 0.61
t.test(x = iq, 
       mu = 69)
# p value = 0.071
t.test(x = iq, 
       mu = 70)
# p value = 0.002104
boxplot(iq$x)
hist(iq$x)
hist(iq$x, breaks = 25)
# no the tests make no sense as there seems to be NO normal distribution


# Set random seed ( reproducible research )
set.seed (1234)
# # Simulate components
SIM <- data.frame ( a = rnorm (200 ,10 ,3) ,b = rweibull (200 ,1 ,10) )
# # Compute three variables
SIM$x.1 <- SIM$a + SIM$b
SIM$x.2 <- 10 * ( SIM$a / SIM$b )
SIM$x.3 <- 10 + SIM$a
# # Summary
summary ( SIM )
# # Box plot ( reshape or simple )
LONG <- reshape( SIM, varying = c("x .1" ,"x .2" ,"x .3" ) , direction = "long")
boxplot (x ~ time , data = LONG , ylim = c (0 ,100) )
boxplot (SIM$x.1 , xlim = c (0 ,4) , ylim = c (0 ,100) )
boxplot (SIM$x.2 , add = TRUE , at =2 )
boxplot (SIM$x.3 , add = TRUE , at =3 )
# # Compare t - test and sign - test
library ( "BSDA" )
SIGN.test (SIM$x.1 , md =20 )
t.test ( SIM$x.1 , mu =20 )
SIGN.test ( SIM$x.2 , md =20 )
t.test ( SIM$x.2 , mu =20 )
SIGN.test ( SIM$x.3 , md =20 )
t.test ( SIM$x.3 , mu =20 )
# # Alternative to SIGN . test () function if BSDA package unavailable
sum( dbinom (0: sum (( SIM$x.3 - 20) >0) , size = length ( SIM$x.3) , prob =0.5) ) * 2

