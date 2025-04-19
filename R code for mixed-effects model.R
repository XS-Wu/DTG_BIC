
# R version 4.4.1
# R studio version 2024.04.2

# Install packages only for first use
# install.packages('splines')
# install.packages('nlme')

# Load packages
library(splines)
library(nlme)


# ----------------------------------------------------------------------------
# Mixed-Effects Model for Blood Glucose Analysis
# ----------------------------------------------------------------------------

# Model Setup:
# Outcome: BloodSugar (blood glucose measurements)
# Fixed Effects: 
#   - Natural cubic spline for time (knot at t=1)
#   - Interaction between time spline and treatment type(ART regimen)
#   - Covariates: age_group, Sex, marital status, transmission route, 
#     WHO stage, region, ART year, time to ART, baseline CD4 group, HIV viral load,
#     baseline blood glucose group
# Random Effects: Individual-specific linear time trajectories (by ID2)

lmm_model <- lme(
  fixed = BloodSugar ~ 
    ns(t, knots = c(1)) +              # Spline for time (1 internal knot)
    ns(t, knots = c(1)):type +         # Time-type interaction
    age_group + Sex + marri + trans +  # Demographic covariates
    WHOStage + region + ARTyear +      # Clinical/regional covariates
    time_to_ART2 + CD4_group +         # Treatment-related covariates
    NaiveVL + Baseline_BloodSugar_group,  # Lab measurements
  
  random = ~ t | ID2,  # Random intercept + slope for time per subject
  method = "REML",     # Restricted Maximum Likelihood
  data = data5         # Dataset containing longitudinal blood glucose data
)

# Model summary
summary(lmm_model)

# Extract standard errors of fixed effects
fixed_se <- summary(lmm_model)$tTable[, "Std.Error"]
