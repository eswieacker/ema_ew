library(tidyverse)
library(ggplot2)
library(lme4)
library(sjPlot)

file_name <- "final_data.csv"

# import data
final_data <- read_csv(file_name)

# remove unnecessary objects
rm(file_name)

# logical regression model glme using lme 4
lme1 <- glmer(exercise ~ overall_na + overall_pa + socialinteraction_yesno +
                (1 | Participant), final_data,
              family = binomial)

tab_model(lme1)

lme2 <- glmer(exercise ~ overall_na + overall_pa + socialinteraction_scale +
                (1 | Participant), final_data,
              family = binomial)

tab_model(lme2)