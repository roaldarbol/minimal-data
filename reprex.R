library(glmmTMB)
library(performance)
library(ggplot2)
library(readr)
library(dplyr)

# See distributions in image below
df_weird <- read_csv('minimal_data.csv')
ggplot(df_weird, aes(value)) +
  geom_histogram() +
  facet_grid(
    rows = vars(condition_b),
    cols = vars(condition_a)
  )

# Fit model
glmm_weird <- glmmTMB::glmmTMB(value ~ condition_a + condition_b + (1 | id),
                               data = df_weird,
                               family = beta_family)

# Check model
performance::check_model(glmm_weird)

# See model performance
performance::model_performance(glmm_weird)
MuMIn::r.squaredGLMM(glmm_weird)
# # Indices of model performance ({performance} < 0.12)
#
# AIC       |      AICc |       BIC | R2 (cond.) | R2 (marg.) |    ICC |  RMSE | Sigma
# ------------------------------------------------------------------------------------
# -1140.006 | -1139.720 | -1106.099 |      1.230 |      1.095 | -1.419 | 0.139 | 6.207

# # Indices of model performance ({performance} v0.12.0.2)
# 
# AIC       |      AICc |       BIC | R2 (cond.) | R2 (marg.) |   ICC |  RMSE | Sigma
# -----------------------------------------------------------------------------------
#   -1140.006 | -1139.720 | -1106.099 |      1.092 |      0.972 | 4.323 | 0.139 | 6.207