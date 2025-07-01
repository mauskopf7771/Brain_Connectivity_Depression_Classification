# New script just for cleaning data, winzorising and normalising variables. 

library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(robustHD)
library(car)

boxplot(df['Neuroticism'], main = 'Neuroticism')
boxplot(df['LCC_Alpha'], main = 'Alpha Clustering Coefficient')
boxplot(df['LCC_Beta'], main = 'Beta Clustering Coefficient')

#Alpha and beta are showing outliers
# So what we can do here is remove them altogether using winzorising then re-run the SVM. 

df['ACC_W'] = winsorize(df['LCC_Alpha'])
df['BCC_W'] = winsorize(df['LCC_Beta'])

boxplot(df['ACC_W'], main = 'ACC_W')
boxplot(df['BCC_W'], main = 'BCC_W')

## With outliers removed, we now need to check for normality. 

# Create Histograms
hist(df$Neuroticism, main = 'Neuroticism')
hist(df$ACC_W, main = 'Alpha Clustering Coefficients')
hist(df$BCC_W, main = 'Beta Clustering Coefficients')

#Shapiro Wilk Tests
shapiro.test(df$Neuroticism) #Normal distribution
shapiro.test(df$ACC_W) #Not normal distribution
shapiro.test(df$BCC_W) #Not normal distribution
#So we either need to fix this by transforming it - makes it very difficult to interpret, or use non-parametric
# Apparently SVMs are robust against non-normality so we're calm on that. 

#What we want to be able to do now is do multiple comparisons
wilcox.test(df$Depression, df$Neuroticism, data = df)
wilcox.test(df$Depression, df$ACC_W, data = df)
wilcox.test(df$Depression, df$BCC_W, data = df)

#Log transform a load of these variables
log_Neuroticism = log(df$Neuroticism)
log_ACC = log(df$ACC_W)
log_BCC = log(df$BCC_W)
hist(log_Neuroticism)
hist(log_ACC)
hist(log_BCC)
shapiro.test(log_Neuroticism)
shapiro.test(log_ACC)
shapiro.test(log_BCC)

sqrt_Neuroticism = sqrt(df$Neuroticism)
shapiro.test(sqrt_Neuroticism)
shapiro.test(df$Neuroticism)
shapiro.test(log_Neuroticism)
log10_Neuroticism = log10(df$Neuroticism)
shapiro.test(log10_Neuroticism)

# Getting normality from ACC
# Log does not work
sqrt_ACC = sqrt(df$ACC_W)
shapiro.test(sqrt_ACC)

library(car)
qqPlot(sqrt_ACC)

#z scores
#z = (x - mean(x)) / sd(x)

# ANOVA tests for this stuff
model <- aov(df$Depression ~ df$Neuroticism * df$LCC_Alpha * df$LCC_Beta, data=df)
summary(model)




##### So for all of these analyses, what we need to look at now is as follows: #####
# Outliers - identified and winzorised - fine
# Normality - clustering coefficients aren't normally distributed - what to do about it?
  #I mean, log transformations aren't working here soooo... I guess what I can do is just use non-parametric
shapiro.test(df$LCC_Alpha)
shapiro.test(df$ACC_W)
shapiro.test(df$LCC_Beta)

# z Score transformations (outliers removed)
hist(df$Neuroticism)
hist(df$ACC_W)
#z = (x - mean(x)) / sd(x)
df$Z_ACC = (df$ACC_W - mean(df$ACC_W)) / sd(df$ACC_W)
hist(df$Z_ACC)
shapiro.test(df$Z_ACC)

hist(df$LCC_Alpha)
df$Z_LCCA = (df$LCC_Alpha - mean(df$LCC_Alpha)) / sd(df$LCC_Alpha)
hist(df$Z_LCCA)

df$Z_LCCB = (df$LCC_Beta - mean(df$LCC_Beta)) / sd(df$LCC_Beta)
hist(df$Z_LCCB)
shapiro.test(df$Z_LCCB)

# Split dataframe by depressed and non-depressed (will be useful to see the distributions)
dfB <- df[which(df$Depression == 0),]
dfC <- df[which(df$Depression == 1),]

hist(dfC$Neuroticism)
shapiro.test(dfC$Neuroticism)

hist(dfB$Neuroticism)
shapiro.test(dfB$Neuroticism)


hist(dfC$LCC_Alpha)
shapiro.test(dfC$LCC_Alpha)
mean(dfC$LCC_Alpha)

hist(dfB$LCC_Alpha)
shapiro.test(df$LCC_Alpha)
mean(df$LCC_Alpha)


#Correlations
cor.test(df$Neuroticism, df$BDI_II, method = 'spearman')
cor.test(df$ACC_W, df$BDI_II, method = 'spearman')
cor.test(df$BCC_W, df$BDI_II, method = 'spearman')

