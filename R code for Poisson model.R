
# R version 4.4.1
# R studio version 2024.04.2

# Install packages only for first use
# install.packages('sandwich')
# install.packages('broom')
# install.packages('lmtest')
# install.packages('nortest')
# install.packages('survival')
# install.packages('survminer')
# install.packages('boot')

# Load packages
library(sandwich)
library(broom)
library(lmtest)
library(nortest)
library(survival)
library(survminer)
library(boot)


# ----------------------------------------------------------------------------
# Adjusted Poisson Regression Model for Mortality Analysis
# ----------------------------------------------------------------------------

# Multivariate Poisson regression model with a time offset and robust variances
# Outcome: event (binary mortality indicator)
# Predictors: type(ART regimen), age_group, Sex, marital status, transmission route, 
#             WHO stage, region, ART year, time to ART, CD4 group, viral load
# Offset: log(follow_up_time) to account for varying follow-up durations

poisson_fit <- glm(
  event ~ type + age_group + Sex + marri + trans + 
    WHOStage + region + ARTyear + time_to_ART2 + 
    CD4_group + NaiveVL, 
  data = data2, 
  offset = log(follow_up_time),  # Offset for follow-up time
  family = poisson(link = 'log')  # Poisson regression with log link
)

# Breusch-Pagan test for heteroscedasticity
bptest(poisson_fit)

# Compute robust standard errors
se <- sqrt(diag(vcovHC(poisson_fit, type = "HC1")))

# Anderson-Darling test for normality
residuals <- resid(poisson_fit, type = "pearson")
ad_test <- ad.test(residuals)
print(ad_test)
