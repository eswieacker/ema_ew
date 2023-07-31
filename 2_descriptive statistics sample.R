# install.packages("haven")
# install.packages("dplyr")
# install.packages("lavaan")
library(haven)
library(dplyr)
library(lavaan)


final_data <- read_csv("final_data.csv")

# calculate mean age
data_age <- final_data %>%
        group_by(Participant) %>%
        summarise(age = mean(Alter))

mean(data_age$age)
print(paste("mean age of participants", mean(data_age$age)))

# calculate range age
min(data_age$age)
max(data_age$age)
print(paste(
        "range age of participants",
        min(data_age$age),
        "-", max(data_age$age)
))

# calculate standard deviation age
sd(data_age$age)
print(paste("standard deviation age of participants", sd(data_age$age)))

# create new data-set for between-person analysis
between_analysis <- final_data %>%
        group_by(Participant) %>%
        # remove char variables
        select(-Form, -exercise_category) %>%
        summarise(across(everything(), ~ mean(., na.rm = TRUE)))

# analyse descriptive statistics BMI Aufnahme
print(paste("BMI mean Aufnahme", mean(between_analysis$BMI_Aufnahme)))
print(paste("BMI sd Aufnahme", sd(between_analysis$BMI_Aufnahme)))
print(paste("BMI range Aufnahme", min(between_analysis$BMI_Aufnahme), "-", max(between_analysis$BMI_Aufnahme))) #nolint

# analyse descriptive statistics BMI EMA
print(paste("BMI mean EMA", mean(between_analysis$BMI_EMA)))
print(paste("BMI sd EMA", sd(between_analysis$BMI_EMA)))
print(paste("BMI range EMA", min(between_analysis$BMI_EMA), "-", max(between_analysis$BMI_EMA))) #nolint

# analyse descriptive statistics SDS Aufnahme
print(paste("SDS mean Aufnahme", mean(between_analysis$SDS_Aufnahme)))
print(paste("SDS sd Aufnahme", sd(between_analysis$SDS_Aufnahme)))
print(paste("SDS range Aufnahme", min(between_analysis$SDS_Aufnahme), "-", max(between_analysis$SDS_Aufnahme))) #nolint

# analyse descriptive statistics SDS EMA
print(paste("SDS mean EMA", mean(between_analysis$SDS_EMA)))
print(paste("SDS sd EMA", sd(between_analysis$SDS_EMA)))
print(paste("SDS range EMA", min(between_analysis$SDS_EMA), "-", max(between_analysis$SDS_EMA))) #nolint

# analyse descriptive statistics treatment duration till EMA
print(paste("Treatment mean EMA", mean(between_analysis$Verweildauer_bis_EMA)))
print(paste("Treatment sd EMA", sd(between_analysis$Verweildauer_bis_EMA)))
print(paste("Treatment range EMA", min(between_analysis$Verweildauer_bis_EMA), "-", max(between_analysis$Verweildauer_bis_EMA))) #nolint

# analyse descriptive statistics diagnoses
diagnoses <- table(between_analysis$Diagnose)
names(diagnoses) <- c("AN_R", "AN_BP", "a_AN")
print(diagnoses)
rel_d <- round(prop.table(diagnoses) * 100, 2)
cum_d <- round(cumsum(prop.table(diagnoses) * 100), 2)
frequencies_diagnoses <- cbind(diagnoses, rel_d, cum_d)
print(frequencies_diagnoses)

# same using dplyr:
diagnoses_alt <- between_analysis %>%
        group_by(Diagnose) %>%
        summarise(count = n()) %>%
        mutate(name = case_when(Diagnose == 1 ~ "AN_R",
                Diagnose == 2 ~ "AN_BP",
                Diagnose == 3 ~ "a_AN"), .after = Diagnose) %>%
        mutate(rel_d = round(count / sum(count) * 100, 2),
                cum_d = cumsum(rel_d))

print(diagnoses_alt)