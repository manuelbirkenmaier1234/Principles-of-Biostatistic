#load libraries
library(librarian)
library(tidyverse)
library(ggplot2)

#start coding
ceiling(rnorm(n=1, mean = 12, sd = 4.5))
general_pop <- ceiling(rnorm(n=2500, mean = 12, sd = 4.5))
mean(sample(general_pop, size = 500, replace = FALSE))

Data <- c (103 , 103 , 103 , 98 , 98 , 98 , 107 , 100 , 112)
t.test ( x = Data,
         mu = 100)
?t.test
