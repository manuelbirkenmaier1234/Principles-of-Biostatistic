#read libraries
library(librarian)
library(tidyverse)
library(ggplot2)
shelf(bsda)
# read datasets
fvc <- read.csv("FVC.csv")
#Questions
#Q4.1 a)
head(fvc)
dim(fvc)
table(fvc$Group)
unique(fvc$Group)

#question4.1 b)
?t.test
t.test(FVC ~ Group, data = fvc, var.equals = FALSE)

#Question4.1 c)
t.test(FVC ~ Group, data = fvc, var.equals = TRUE)$p.value



#question 4.1 d)
fvc$Group <- as.factor(fvc$Group)
class(fvc$Group)
boxplot(FVC ~ Group, data = fvc, )
histo.fvc <- ggplot( data = fvc, mapping = aes(x= FVC, fill = Group)) + 
            geom_histogram(bins = 10) + 
            facet_grid(rows = vars(Group)) + 
            labs(title = "FVC count by sex",
                 x = "FVC (l)")
histo.fvc

#Question4.2


#Question 4.3
#mean depression score.coun = 70 [95% CI: 69.1-71]
#mean deprs score.no.coun = 78.5 [95% CI: 77.5 - 79.6]

#perform a test of difference between the two groups
# with "tsum.test" we can enter the summary statistic (no raw data)
# we need to calculate the SD for the two samples (assuming normal distribution)

# CI = x +/- z * SE
# SE = SD / sqrt(n) ---> SD = SE * sqrt(n)
# 69.1 = 70 - 1.96 * SE --> SE = (70-69.1) / 1.96
SE1 <- (70-69.1) / 1.96
SD1 <- SE1 * sqrt(100)
SD1

SE2 <- (78.5 - 77.5) / 1.96
SD2 <- SE2 * sqrt(100)
SD2
tsum.test(70,
          s.x = SD1,
          n.x = 100,
          mean.y = 78.5,
          s.y = SD2,
          n.y = 100,
          alternative = "two.sided",
          mu = 0,
          var.equal = FALSE,
          conf.level = 0.95)


#Question 4.4
compar <- read.csv("Comparisons.csv")
head(compar)
dim(compar)
?t.test
compar$Group <- as.factor(compar$Group)
factor(compar$Group)
unique(compar$Group)
table(compar$Group)
compar.ab <- subset(compar, Group == "A" | Group == "B")
unique(compar.ab$Group)
table(compar.ab$Group)

t.test(Score ~ Group, data = compar.ab, na.action = na.pass, var.equals = FALSE)
#p value > 5 % and CI spans the 0 so there is evidence not to reject the Nullhypothesis

compar.bc <- subset(compar, Group == "B" | Group == "C")
unique(compar.bc$Group)
t.test(Score ~ Group, data = compar.bc, na.action = na.pass, var.equals = FALSE)
# p value > 5% and CI spans the 0 so there is evidence not to reject the Nullhypothesis

compar.ac <- subset(compar, Group == "A" | Group == "C")
unique(compar.bc$Group)
t.test(Score ~ Group, data = compar.ac, na.action = na.pass, var.equals = FALSE)

#e) in our statistical testing we did not find strong enough evidence to reject the nullhypothesis
#we did not reject the Nullhypothesis (no difference betweeen A and B AND B and C)
#however, this does not let us conclude anything about the groups A and C
#statistical testing found evidence to reject H0, suggesting that there in fact tehre is
# a difference between A and C
boxplot(Score ~ Group, data=compar)

#Questions 4.5
#a)
heart <- read.csv("Heart.Monitor.csv")
head(heart)
boxplot(heart, data=heart)
t.test(x = heart$Old, y = heart$New, 
       alternative = "t",
       mu = 0, paired = TRUE, var.equals = FALSE,
       conf.level = 0.95)
# in paired t test we use x - y, so old - new: there is evidence against
# the H0 (at 5% level) that the two monitors measure the same heart rate 
# OR: shows a significant (at 5% level) difference between two monitors
#with mean = -1.41 [95% CI -1.96 to -0.85]

Data <- read.csv("Heart.Monitor.csv")

difference <- Data$Old - Data$New
average <- (Data$Old + Data$New) / 2

bias <- mean(difference)
sd_diff <- sd(difference)

lower <- bias - 1.96 * sd_diff
upper <- bias + 1.96 * sd_diff
plot(average, difference,
     xlab = "Average heart rate (bpm)",
     ylab = "Old - New (bpm)",
     main = "Bland-Altman Plot")

abline(h = bias, col = "blue", lwd = 2)
abline(h = c(lower, upper),
       col = "red", lty = 2)
abline(h = 0, col = "gray")

#Question4.5 
#b)
