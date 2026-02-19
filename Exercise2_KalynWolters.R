#GEO411 - Homework 2
#Kalyn Wolters 
#Analysis of variance

precipSample <- read.csv("precipSample (1).csv")
precipSample_1_
#creating periods for different time frames of samples
precipSample$Period[(precipSample$Year <= 1967)] <- "Period1" 
precipSample$Period[(precipSample$Year > 1967 & precipSample$Year < 1997)] <- "Period2"
precipSample$Period[(precipSample$Year >= 1997)] <- "Period3"
precipSample

#Buffalo Analyses

#table for sample size
table(precipSample$Period) 
n1 <- sum(precipSample$Period == "Period1") 
n2 <- sum(precipSample$Period == "Period2") 
n3 <- sum(precipSample$Period == "Period3") 
n <- length(precipSample$Buffalo)
#variable for number of categories 
k <- 3 

#Calculating sample means based on periods 
mean1 <- mean(precipSample$Buffalo[precipSample$Period == 
                                     "Period1"]) 
mean2 <- mean(precipSample$Buffalo[precipSample$Period == 
                                     "Period2"]) 
mean3 <- mean(precipSample$Buffalo[precipSample$Period == 
                                     "Period3"]) 
mean1

#adding text to label quantities
print("sample means") 
print(paste("Overall",mean(precipSample$Buffalo))) 
print(paste("Period1",mean1)) 
print(paste("Period2",mean2)) 
print(paste("Period3",mean3)) 

#boxplot  for examining the similarities and differences between distributions
boxplot(Buffalo ~ Period, data = precipSample, ylab = "annual precipitation (in.)") 

layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$Buffalo),max(precipSample$Buffalo)+1)
BuffaloPrecipBreaks <- seq(22,54,2)
hist(precipSample$Buffalo[precipSample$Period == "Period1"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
hist(precipSample$Buffalo[precipSample$Period == "Period2"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
hist(precipSample$Buffalo[precipSample$Period == "Period3"], breaks = BuffaloPrecipBreaks, xlim = minMax, ylim = c(0,5), main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
layout(matrix(c(1)))

#Critical value
Fcrit <- qf(0.05, (k-1), (27), lower.tail = FALSE) 
Fcrit 
#ANOVA
modelBuffalo <- lm(Buffalo ~ as.factor(Period), data = precipSample) 
anova(modelBuffalo) 

#Levene test Buffalo
precipSample$BuffaloAbsoluteDeviations <- abs(resid(modelBuffalo))
precipSample
boxplot(precipSample$BuffaloAbsoluteDeviations ~ Period, data = precipSample)
anova(lm(precipSample$BuffaloAbsoluteDeviations ~ as.factor(precipSample$Period)))

#Boxplot to check variation in each time period
boxplot(precipSample$BuffaloAbsoluteDeviations ~ Period, 
        data = precipSample) 
#Normality Plots
layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$Buffalo),max(precipSample$Buffalo)+1)
BuffaloPrecipBreaks <- seq(22,54,2)
hist(precipSample$Buffalo[precipSample$Period == "Period1"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period1"])), add=TRUE)
hist(precipSample$Buffalo[precipSample$Period == "Period2"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period2"])), add=TRUE)
hist(precipSample$Buffalo[precipSample$Period == "Period3"], freq = FALSE, breaks = BuffaloPrecipBreaks, xlim = minMax, main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period3"])), add=TRUE)
layout(matrix(c(1)))

# K-S test Buffalo
#Period 1
period1ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period1"])
plot(period1ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period1"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period1"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period1"]), sd(precipSample$Buffalo[precipSample$Period == "Period1"]))

#Period 2
period2ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period2"])
plot(period2ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period2"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period2"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period2"]), sd(precipSample$Buffalo[precipSample$Period == "Period2"]))

#Period 3
period3ECDF <- ecdf(precipSample$Buffalo[precipSample$Period == "Period3"])
plot(period3ECDF)
curve(pnorm(x, mean=mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd=sd(precipSample$Buffalo[precipSample$Period == "Period3"])), add=TRUE)
ks.test(precipSample$Buffalo[precipSample$Period == "Period3"], "pnorm", mean(precipSample$Buffalo[precipSample$Period == "Period3"]), sd(precipSample$Buffalo[precipSample$Period == "Period3"]))
#K-W test
kruskalWallis <- kruskal.test(Buffalo ~ as.factor(Period),data = precipSample) 
kruskalWallis 

qchisq(0.05, k-1, lower.tail = FALSE)

#Median Test Buffalo
medianPrecipBuffalo <- median(precipSample$Buffalo)
medianPrecipBuffalo

boxplot(Buffalo ~ Period, data = precipSample, ylab = "annual precipitation (in.)")
abline(h = medianPrecipBuffalo, lwd = 2)

precipSample$BuffaloMedian <- ifelse(precipSample$Buffalo > medianPrecipBuffalo, "Greater than median", "Less than or equal")
precipSample
medianTable <- table(precipSample$BuffaloMedian, precipSample$Period)
medianTable
chisq <- chisq.test(medianTable)
chisq$expected
chisq


#San Diego Analyses

##Using R to calculate means
mean1 <- mean(precipSample$SanDiego[precipSample$Period == "Period1"])
mean2 <- mean(precipSample$SanDiego[precipSample$Period == "Period2"])
mean3 <- mean(precipSample$SanDiego[precipSample$Period == "Period3"])

print("sample means")
print(paste("Overall",mean(precipSample$SanDiego)))
print(paste("Period1",mean1))
print(paste("Period2",mean2))
print(paste("Period3",mean3))

#Visualizing ANOVA

boxplot(SanDiego ~ Period, data = precipSample, ylab = "annual precipitation (in.)")

layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$SanDiego),max(precipSample$SanDiego))
SanDiegoPrecipBreaks <- seq(3,25,2)
hist(precipSample$SanDiego[precipSample$Period == "Period1"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
hist(precipSample$SanDiego[precipSample$Period == "Period2"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
hist(precipSample$SanDiego[precipSample$Period == "Period3"], breaks = SanDiegoPrecipBreaks, xlim = minMax, ylim = c(0,6), main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
layout(matrix(c(1)))

#Find the critical value 
Fcrit <- qf(0.05, (k-1), (27), lower.tail = FALSE)
Fcrit

#The Quick Way to do ANOVA
modelSanDiego <- lm(SanDiego ~ as.factor(Period), data = precipSample)
anova(modelSanDiego)


#Levene Test San Diego
precipSample$SanDiegoAbsoluteDeviations <- abs(resid(modelSanDiego))
precipSample
boxplot(precipSample$SanDiegoAbsoluteDeviations ~ Period, data = precipSample)
anova(lm(precipSample$SanDiegoAbsoluteDeviations ~ as.factor(precipSample$Period)))

#Normality Plots
layout(matrix(c(1,2,3)))
minMax <- c(min(precipSample$SanDiego),max(precipSample$SanDiego))
SanDiegoPrecipBreaks <- seq(3,25,2)
hist(precipSample$SanDiego[precipSample$Period == "Period1"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 1", xlab = "Annual Precipitation (in.)")
abline(v = mean1, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period1"])), add=TRUE)
hist(precipSample$SanDiego[precipSample$Period == "Period2"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 2", xlab = "Annual Precipitation (in.)")
abline(v = mean2, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period2"])), add=TRUE)
hist(precipSample$SanDiego[precipSample$Period == "Period3"], freq = FALSE, breaks = SanDiegoPrecipBreaks, xlim = minMax, main = "Period 3", xlab = "Annual Precipitation (in.)")
abline(v = mean3, lwd = 2)
curve(dnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period3"])), add=TRUE)
layout(matrix(c(1)))

# K-S test
#Period 1
period1ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period1"])
plot(period1ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period1"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period1"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period1"]), sd(precipSample$SanDiego[precipSample$Period == "Period1"]))

#Period 2
period2ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period2"])
plot(period2ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period2"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period2"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period2"]), sd(precipSample$SanDiego[precipSample$Period == "Period2"]))

#Period 3
period3ECDF <- ecdf(precipSample$SanDiego[precipSample$Period == "Period3"])
plot(period3ECDF)
curve(pnorm(x, mean=mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd=sd(precipSample$SanDiego[precipSample$Period == "Period3"])), add=TRUE)
ks.test(precipSample$SanDiego[precipSample$Period == "Period3"], "pnorm", mean(precipSample$SanDiego[precipSample$Period == "Period3"]), sd(precipSample$SanDiego[precipSample$Period == "Period3"]))

#Kruskal-Wallis San Diego
kruskalWallis <- kruskal.test(SanDiego ~ as.factor(Period), data = precipSample)
kruskalWallis

qchisq(0.05, k-1, lower.tail = FALSE)

#Median Test San Diego
medianPrecipSanDiego <- median(precipSample$SanDiego)
medianPrecipSanDiego

boxplot(SanDiego ~ Period, data = precipSample, ylab = "annual precipitation (in.)")
abline(h = medianPrecipSanDiego, lwd = 2)

precipSample$SanDiegoMedian <- ifelse(precipSample$SanDiego > medianPrecipSanDiego, "Greater than median", "Less than or equal")
precipSample
medianTable <- table(precipSample$SanDiegoMedian, precipSample$Period)
medianTable
chisq <- chisq.test(medianTable)
chisq$expected
chisq

