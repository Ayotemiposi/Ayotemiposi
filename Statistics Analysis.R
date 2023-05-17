setwd("C:/Users/admin/OneDrive - University of Salford")
WDI <- read.csv("World_Development_Indicator.csv")
summary(WDI)
library(readr)
WDI <- read_csv("World_Development_Indicator.csv", 
                col_types = cols(Time = col_character(), 
                                 `Wage and salaried workers, total (% of total employment) (modeled ILO estimate) [SL.EMP.WORK.ZS]` = col_number(), 
                                 `Self-employed, total (% of total employment) (modeled ILO estimate) [SL.EMP.SELF.ZS]` = col_number(), 
                                 `Imports of goods and services (current LCU) [NE.IMP.GNFS.CN]` = col_number(), 
                                 `Insurance and financial services (% of commercial service exports) [TX.VAL.INSF.ZS.WT]` = col_number(), 
                                 `Exports of goods and services (current US$) [NE.EXP.GNFS.CD]` = col_number()))
View(WDI)
summary(WDI)
str(WDI)
head(WDI)
tail(WDI)
WDI <- WDI[-c(151:155),]
colnames(WDI)
colnames(WDI)[colnames(WDI) %in% 
                c("Country Name","Country Code","Time","Time Code",
                  "Inflation, consumer prices (annual %) [FP.CPI.TOTL.ZG]",
                  "GDP (current LCU) [NY.GDP.MKTP.CN]","GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]",
                  "Unemployment, total (% of total labor force) (modeled ILO estimate) [SL.UEM.TOTL.ZS]",
                  "Wage and salaried workers, total (% of total employment) (modeled ILO estimate) [SL.EMP.WORK.ZS]",
                  "Self-employed, total (% of total employment) (modeled ILO estimate) [SL.EMP.SELF.ZS]",
                  "Imports of goods and services (current LCU) [NE.IMP.GNFS.CN]",
                  "Insurance and financial services (% of commercial service exports) [TX.VAL.INSF.ZS.WT]",
                  "Exports of goods and services (current US$) [NE.EXP.GNFS.CD]")] <- 
  c("Country","C-Code","Year","Y-Code","Inflation","GDP", "GDP growth","Unemployment","Wage and salaried workers",
    "Self-employed","Imports","Insurance and financial services","Export")

####Explore the dataset####
install.packages("DataExplorer")
library(DataExplorer)
create_report(WDI)
WDI_2011 <- WDI[WDI$Year==2011,]
WDI_2011

###Plotting WDI_2011 using names.arg for Inflation to select 10 countries###
barplot(WDI_2011$Inflation,names.arg = WDI_2011$`C-Code`)

WDI_New <- WDI[-c(11:30,51:60,81:90,111:120),]
View(WDI_New)

####DESCRIPTIVE AND STATISTICAL ANALYSIS#######
#####To generate the mean and standard deviation of a dataset#####
install.packages("skimr")
library(skimr)
skim(WDI_New)

##OR
install.packages("mosaic")
library(mosaic)
dfapply(WDI_New,favstats)
colnames(WDI_New)[colSums(is.na(WDI_New)) > 0]

WDI_New$`Wage and salaried workers`[is.na(WDI_New$`Wage and salaried workers`)] <- mean(WDI_New$`Wage and salaried workers`, na.rm = TRUE)
WDI_New$`Wage and salaried workers`

WDI_New$`Self-employed`[is.na(WDI_New$`Self-employed`)] <- mean(WDI_New$`Self-employed`, na.rm = TRUE)
WDI_New$`Self-employed`

WDI_New$Imports[is.na(WDI_New$Imports)] <- mean(WDI_New$Imports, na.rm = TRUE)
WDI_New$Imports

WDI_New$Export[is.na(WDI_New$Export)] <- mean(WDI_New$Export, na.rm = TRUE)
WDI_New$Export

###Let's create a new report on the clean dataset exluding country, country code, year code and year###
WDI.New <- WDI_New[,!(names(WDI_New) %in% c("Country","C-Code","Year","Y-Code"))]
create_report(WDI.New)
####The Mode of the dataset cannot be calculated because the variables are continuous
###Checking skewness and kurtosis
install.packages("moments")
library(moments)

skewness(WDI.New)

kurtosis(WDI.New)

####GRAPHICAL EXPLORATION OF THE DATASET####
plot(WDI_New)

hist(WDI_New$Inflation)

hist(WDI_New$GDP)

plot(density(WDI_New$Inflation))

plot(density(WDI_New$GDP))

###Using boxplot to identify outliers##

boxplot(WDI_New$Inflation~WDI_New$`C-Code`,
        main='Boxplot of Inflation by Country',ylab='Inflation',
        col= rainbow(10))->b_Inflation_Country

b_Inflation_Country$out

boxplot(WDI_New$GDP~WDI_New$`C-Code`,
        main='Boxplot of GDP by Country',ylab='GDP',
        col= rainbow(10))->b_GDP_Country

boxplot(WDI_New$GDP~WDI_New$`C-Code`,
        main='Boxplot of GDP by Country',ylab='GDP',
        col= rainbow(4),
        subset= WDI_New$`C-Code` %in% c("BRA","FIN","GRC","GBR"))

boxplot(WDI_New$GDP~WDI_New$`C-Code`,
        main='Boxplot of GDP by Country',ylab='GDP',
        col= rainbow(2),
        subset= WDI_New$`C-Code` %in% c("FIN","GRC"))

###To identify the outliers
b_GDP_Country$out

###Using Pie Function to evaluate each country in 2020
WDI_2020 <- WDI_New[WDI_New$Year==2020,]
WDI_2020

pie(WDI_2020$GDP, labels=WDI_2020$Country,col=rainbow(10))

percent <- round(100*WDI_2020$GDP/sum(WDI_2020$GDP),1)
percent <- paste(WDI_2020$`C-Code`, "-",percent,"%")
pie(WDI_2020$GDP, labels=percent,col=rainbow(10))

###ASSESSING NORMALITY OF THE DATASET
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
library("datarium")
library("qqplotr")
library("ggplot2")

ggplot(mapping=aes(sample=WDI_New$Inflation)) + 
  stat_qq_point(size=2,color="blue") +
  stat_qq_line(color="orange")

ggplot(mapping=aes(sample=WDI_New$GDP)) + 
  stat_qq_point(size=2,color="blue") +
  stat_qq_line(color="orange")

qplot(`C-Code`,Inflation,data=WDI_New,color = Year)

###CORRELATION ANALYSIS BETWEEN THE TWO VARIABLES###
##Pearson correlation
cor(WDI_New$Inflation,WDI_New$GDP)

##Spearman correlation
cor(WDI_New$Inflation,WDI_New$GDP, method = "spearman")

###Correlation of all variables to two decimals
round(cor(WDI.New), digits = 2)

##Improved correlation matrix
library("corrplot")
corrplot(cor(WDI.New), method = "number", type = "upper")

corrplot(cor(WDI.New))
######REGRESSION ANALYSIS######
install.packages("car")
install.packages("caret")
library("car")
library("caret")

#Y = GDP
#X = Inflation

model_1 <- lm(GDP~Inflation,WDI.New)
summary.lm(model_1)

###Drawing the scatter plot and fitted regression line
plot(GDP~Inflation,WDI.New, col="blue",
     main = "Regression: GDP and Inflation",
     xlab = "Inflation",
     ylab = "GDP")
abline(model_1,col="red")

####Checking Assumptions of the model
##Linearity
##Residuals' Independence:The red line is not approximately horizontal at zero
plot(model_1,1)
##Normality of residuals
plot(model_1,2)
###Equal variances of the residuals(Homoscedasticity)
plot(model_1,3)

#Y = GDP
#X = Self-employed

model_1a <- lm(GDP~`Self-employed`,WDI.New)
summary.lm(model_1a)

###Drawing the scatter plot and fitted regression line
plot(GDP~`Self-employed`,WDI.New, col="blue",
     main = "Regression: GDP and Self-employed",
     xlab = "Self-employed",
     ylab = "GDP")
abline(model_1a,col="red")

####Checking Assumptions of the model
##Linearity
##Residuals' Independence
plot(model_1a,1)
##Normality of residuals
plot(model_1a,2)
###Equal variances of the residuals(Homoscedasticity)
plot(model_1a,3)

###PERFORMING MULTIPLE LINEAR REGRESSION MODEL###
## Y = GDP
## X1 = Inflation
## X2 = Self-employed

model_2 = lm(GDP~Inflation + `Self-employed` ,WDI.New)
summary.lm(model_2)

## Y = GDP
## X1 = Inflation
## X2 = Self-employed
## X3 = Imports

model_3 = lm(GDP~Inflation + `Self-employed` + Imports ,WDI.New)
summary.lm(model_3)

## Y = GDP
## X1 = Inflation
## X2 = Self-employed
## X3 = Imports
## X4 = Export

model_4 = lm(GDP~Inflation + `Self-employed` + Imports + Export,WDI.New)
summary.lm(model_4)

## Y = GDP
## X1 = Inflation
## X2 = Imports
## X3 = Export

model_5 = lm(GDP~Inflation + Imports + Export,WDI.New)
summary.lm(model_5)

## Y = GDP
## X1 = Inflation
## X2 = Imports

model_6 = lm(GDP~Inflation + Imports,WDI.New)
summary.lm(model_6)

###Assumptions of the model
##Linearity:
colnames(WDI.New)
pairs(WDI.New[,c(2,1,7,9)], lower.panel = NULL, pch =19, cex = 0.2)

##Residuals' Independence
plot(model_5,1)

##Normality of residuals
plot(model_5,2)

##Homoscedasticity:
plot(model_5,3)

##No Multicolinearity
vif(model_5)

#############PERFORMING TIME SERIES ANALYSIS###############
install.packages("TTR")
install.packages("forecast")
library(TTR)
library(forecast)

#Running time series setting frequency at 12
Inf_series<- ts(WDI_New$Inflation,frequency=12,start=(2011),end=(2020))
Inf_series
plot.ts(Inf_series)

####DECOMPOSING SEASONAL TIME SERIES####
Inf_seriescomp <- decompose(Inf_series)
Inf_seriescomp$seasonal
plot(Inf_seriescomp)

#Seasonally adjusting
S_adjust <- Inf_series - Inf_seriescomp$seasonal
plot(S_adjust)

#FORECASTING#
library("forecast")
#We need to define the function
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) {mymin <- mymin2}
  if (mymax2 > mymax) {mymax <- mymax2}
  #make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#Holt-Winters Exponential Smoothing(additive model with increasing or decreasing trend and seasonality)
Inf_seriesforecasts <- HoltWinters(Inf_series)
Inf_seriesforecasts
plot(Inf_seriesforecasts)
Inf_seriesforecasts$SSE
##Plotting more months and years aside the original data, January 2021 to December 2025
Inf_seriesforecasts2 <- forecast(Inf_seriesforecasts, h=60)
plot(Inf_seriesforecasts2)

#Carrying out Ljung-Box test and making a correlogram
length(Inf_seriesforecasts2) #check the length of the time series and set as maximum lag
acf(Inf_seriesforecasts2$residuals, lag.max = 10, na.action = na.pass)
Box.test(Inf_seriesforecasts2$residuals, lag = 10, type="Ljung-Box")
#This was giving decimal result, lets try the capital Acf(capital A) in the package "forecast"
Acf(Inf_seriesforecasts2$residuals, lag.max = 10, na.action = na.pass)
Box.test(Inf_seriesforecasts2$residuals, lag = 10, type="Ljung-Box")

#make time series plot
plot.ts(Inf_seriesforecasts2$residuals)
Inf_seriesforecasts2$residuals <- Inf_seriesforecasts2$residuals[
  !is.na(Inf_seriesforecasts2$residuals)]
#plot the histogram
plotForecastErrors(Inf_seriesforecasts2$residuals)

##ARIMA MODELS##
#Plot the time series to check if it is stationary
plot.ts(Inf_series)
#lets difference the time series
Infseriesdiff <- diff(Inf_series, differences=1)
plot.ts(Infseriesdiff)

#plot a correlogram
Acf(Infseriesdiff, lag.max = 10)

#get the autocorrelation values
Acf(Infseriesdiff, lag.max = 10, plot=FALSE)
#We can see from the correlogram that the autocorrelation at lag 3(-0.232) exceeds significance bounds,
#but all other autocorrelations between lags 0 -3 and 3-10 do not exceed the significance bounds.

#Plot the partialcorrelation
Pacf(Infseriesdiff, lag.max = 10)
Pacf(Infseriesdiff, lag.max = 10, plot=FALSE)

auto.arima(WDI_New$Inflation)

auto.arima(Infseriesdiff)

#Forecasting using Arima Models
#fitting an ARIMA(1,1,1)model
Inf_seriesArima <- arima(Inf_series, order=c(1,1,1))
Inf_seriesArima

Inf_seriesforecasts3 <- forecast(Inf_seriesArima, h=12)
Inf_seriesforecasts3

#Plot the forecasts
plot(Inf_seriesforecasts3)

#Perform Ljung-Box test
Acf(Inf_seriesforecasts3$residuals, lag.max = 10)
Box.test(Inf_seriesforecasts3$residuals, lag=10, type="Ljung-Box")

#Time plot forecast error
plot.ts(Inf_seriesforecasts3$residuals)

#Make a histogram
plotForecastErrors(Inf_seriesforecasts3$residuals)

####HYPOTHESIS TEST####
##Using Shapiro-Wilk test and Kolmogorov-Smirnov test to access the dataset
shapiro.test(WDI_New$Inflation)
#The p-value of the test is 5.795e-06 which is less than 0.05 meaning that the data is not normally distributed

shapiro.test(WDI_New$GDP)
#The p-value of the test is 1.538e-10 which is less than 0.05 meaning that the data is not normally distributed

##Using Kolmogorov-Smirnov test for the distribution
ks.test(WDI_New$Inflation,'pnorm')
###The p-value < 2.2e-16, which is less than 0.05,meaning that the data is not normally distributed

ks.test(WDI.New$GDP,'pnorm')
##The p-value is same as that with Inflation

##NORMALIZING THE DATASET
##1. Log Transformation
log_Inflation <- log10(WDI_New$Inflation)
#histogram for both distribution
##original distribution
hist(WDI_New$Inflation, col='steelblue', main='Original')

##log-transformed distribution
hist(log_Inflation, col='coral2', main='Log Transformed')
###The result showed that NA was produced in the process hence we will ignore Log transformation process
##But let's perform shapiro-wilk test on the log transformed data
shapiro.test(log_Inflation)
###p-value is 0.00255 which is still less than 0.05 which indicates that the dataset is not normally distributed

##2. Square Root Transformation
sqrt_Inflation <- sqrt(WDI_New$Inflation)
##The result showed that NA was produced but let's view with histogram
#histogram for both distribution
##original distribution
hist(WDI_New$Inflation, col='steelblue', main='Original')

##Square-root transformed distribution
hist(sqrt_Inflation, col='coral2', main='Square-Root Transformed')
###The result showed that NA was produced but not affecting the distribution
##But let's perform shapiro-wilk test on the square-root transformed data
shapiro.test(sqrt_Inflation)
###p-value is 0.01987 which is still less than 0.05 which indicates that the dataset is not normally distributed

##2. Cube Root Transformation
cube_Inflation <- WDI_New$Inflation^(1/3)
##The result showed that there was no NA produced but let's view with histogram
#histogram for both distribution
##original distribution
hist(WDI_New$Inflation, col='steelblue', main='Original')

##Cube-root transformed distribution
hist(cube_Inflation, col='coral2', main='Cube-Root Transformed')
###The result showed a more normally distributed dataset
##But let's perform shapiro-wilk test on the cube-root transformed data
shapiro.test(cube_Inflation)
###p-value is 0.169 which is more than 0.05 which indicates that the dataset is now normally distributed
##The Cube-Root Transformation method to Normalize data is the best fit for this dataset

###PERFORMING T-TEST###
##Insert the normalized data of Inflation into the dataset
WDI_New2 <- WDI_New[,-5]
WDI_New2$Inflation <- cube_Inflation
View(WDI_New2)

##H0: Less developed countries has the highest inflation rate
##H1: Less developed countries does not have the highest inflation rate
##Common p-value: p < 0.05
install.packages("car")
install.packages("corrplot")
install.packages("dplyr")
library("car")
library("corrplot")
library("dplyr")

###ONE SAMPLE T-TEST
t.test(WDI_New2$Inflation)
##The p-value is 2.2e-16 which is less than 0.05, so we will reject the null hypothesis
##that less developed countries has the highest inflation rate

###INDEPENDENT TWO SAMPLE T-TEST
str(WDI_New2)
###convert Country to two levels of more developed and less developed countries
###Then create a new column with the country code column using "sapply" and "switch" function
WDI_New2$Country_Status <- sapply(WDI_New2$`C-Code`, switch, "GBR" = 'More_Developed',
                                  "USA" = 'More_Developed', "BRA" = 'More_Developed',
                                  "CHN" = 'More_Developed', "GRC" = 'More_Developed',
                                  "FIN" = 'More_Developed', "RUS" = 'More_Developed',
                                  "TUR" = 'More_Developed', "NGA" = 'Less_Developed',
                                  "IND" = 'Less_Developed')
View(WDI_New2)
##Check the structure of the new variable and convert to a factor
str(WDI_New2$Country_Status)
WDI_New2$Country_Status <- as.factor(WDI_New2$Country_Status)

###Now I Will export my dataset as csv###
write.csv(WDI_New2,"WDI_New5.csv",row.names=FALSE)

###Check if the exported dataset is readable###
New <- read.csv("WDI_New5.csv")
##Using box plot to compare More developed and Less developed
boxplot(Inflation~`Country_Status`,data=WDI_New2, names=c("More_Developed", "Less_Developed"),
        xlab = "More developed or Less developed", ylab= "Inflation rate",
        main = "Inflation rate for both More developed and Less developed Countries")

##Using two sample t-test using the same function
t.test(Inflation~`Country_Status`,WDI_New2)
##p-value is still less than 0.05, so we will reject the null hypothesis

##Using a one tail test
t.test(Inflation~`Country_Status`,WDI_New2, alternative="less")
#the p-value is 1 which is way higher than 0.05, then we will not reject the null hypothesis
##given that that it is a one tail or lower-tail test

##Because the assumptions is not met using t-test, we need to run a
##Non-parametric alternatives to T-tests
##Using Mann-Whitney
##Let's visually check for normality
M_developed <- WDI_New2$Inflation[WDI_New2$Country_Status=="More_Developed"]

ggplot(mapping = aes(sample = M_developed)) +
  stat_qq_point(size=2,color="blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

options(scipen=999)
hist(M_developed)

L_developed <- WDI_New2$Inflation[WDI_New2$Country_Status=="Less_Developed"]

ggplot(mapping = aes(sample = L_developed)) +
  stat_qq_point(size=2,color="blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

hist(L_developed)
##Running the hypothesis test
wilcox.test(Inflation~`Country_Status`,WDI_New2)

##P-value is 1.827e-06, which is less than 0.05, so we will still reject the null hypothesis

