library(MASS)
library(moments)
library(lmtest)
library(carData)
library(predict3d)
library(faraway)
library(ggplot2)
library(stats)
library(tidyverse)


## 1. Analyze:
## categorical variables (looking at the distribution by categories),
## numerical variables (looking at the trend of values and the relationships between variables)

## 2. Visualize variables and interpret graphs.


# Dataset overview

library(readxl)
dataTAB <- read_excel("D:/Master & Other stuff/Crystal System Data Science&Engineering/Case Study/Tobacco_Consumption.xlsx", 
                                  col_types = c("numeric", "text", "text", 
                                                "numeric", "text", "text", "text", 
                                                "text", "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))
View(dataTAB)


dataTABdf <- data.frame(dataTAB)


# Read data description

head(dataTABdf) # first six rows
tail(dataTABdf) # last six rows
nrow(dataTABdf) # nr of rows
ncol(dataTABdf) # nr of columns 
dim(dataTABdf) # nr of rows and nr of columns (dimensions)

summary(dataTABdf) # also helps checking for NA's


# Split variables based on their specifics

library(dplyr)
glimpse(dataTABdf)

library(skimr)
skim(dataTABdf)

# Make statistical test to understand relationships between variables

# simple version of a correlation plot
df1 <- data.frame(dataTABdf[9], dataTABdf[10], dataTABdf[11])
df2 <- data.frame(dataTABdf[12], dataTABdf[13], dataTABdf[14])
pairs(df1,col = "blue")
pairs(df2,col = "blue")

# advanced versions of a correlation plot
library(psych)
pairs.panels(df1)
pairs.panels(df2)
library(GGally)
ggpairs(dataTABdf)

# applying a Chi-square statistical test: a p-value < 0.05 suggests a significant correlation between the two variables
print(chisq.test(dataTABdf$Domestic, dataTABdf$Imports))

# checking the value of the correlation between the two variables
cor(dataTABdf$Domestic, dataTABdf$Imports)
## (dataTABdf$`Domestic Per Capita`, dataTABdf$`Total Per Capita`)

# applying the correlation function from the correlation package+library
library(correlation)
cor1 <- correlation(df1)
cor1


# EDA and preprocessing

# checking for NA's (missing values in the dataset)
library(naniar)

# Are there NA's in the dataset?
any_na(dataTABdf)

# checking for duplicates in the rows of the dataset
sum(duplicated(dataTABdf))


# Check data distribution, handle with outliers

# understanding the distribution of the data

table(dataTABdf$Topic)/nrow(dataTABdf)
table(dataTABdf$Measure)/nrow(dataTABdf)
table(dataTABdf$Submeasure)/nrow(dataTABdf)

library(forecast)
library(fpp2)

## after identifying the outliers, DO SOMETHING ABOUT IT !!!
par(mfrow=c(1,3))
boxplot(dataTABtb$Domestic)
boxplot(dataTABtb$Imports)
boxplot(dataTABtb$Total)

par(mfrow=c(1,3))
boxplot(dataTABtb$`Domestic Per Capita`)
boxplot(dataTABtb$`Imports Per Capita`)
boxplot(dataTABtb$`Total Per Capita`)

par(mfrow=c(1,3))
hist(dataTABtb$Domestic)
hist(dataTABtb$Imports)
hist(dataTABtb$Total)

par(mfrow=c(1,3))
hist(dataTABtb$`Domestic Per Capita`)
hist(dataTABtb$`Imports Per Capita`)
hist(dataTABtb$`Total Per Capita`)

ggplot(dataTABdf, aes(x= Measure, fill = Submeasure)) + geom_bar() +
  ggtitle("The distribution of Tobacco Measures by Submeasures")


ggplot(dataTABdf, aes(x=Year, y=Domestic)) + geom_line( color="blue", size=2, alpha=0.9, linetype=2) +
ggtitle("Evolution of Tobacco Consumption over the Years")

  