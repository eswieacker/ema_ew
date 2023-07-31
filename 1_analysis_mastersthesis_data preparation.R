library(tidyverse)
library(haven)
library(lavaan)
library(magrittr)
library(glue)

file_name <- "datensatz_masterarbeit_final.sav"

## import data ####
data_ema_ed <- read_sav(file_name)


## create new subset with variables relevant for analysis ####
data_analysis <- data_ema_ed %>%

        # keep only first 120 columns
        select(1:120) %>%

        # exclude EC data because of missing variables
        filter(Form != "Event-FB") %>%

        # get rid of prefix "reason_" for consistency
        rename_with(~str_replace(., "^reason_", "")) %>%

        # rename misspelled var name
        rename(disgustwithself = disgustedwithmyself)


# remove unnecessary objects
rm(data_ema_ed, file_name)


## prepare descriptive statistics missing data ####

table_descriptive <- data_analysis %>%

        # tabulate count numbers of categories of Form
        count(Participant, Form) %>%
        # change output format
        pivot_wider(names_from = Form, # event type names
                values_from = n, # count of event type per participant
                values_fill = 0) # change NA values to 0

table_descriptive %<>%

        # get a col cum (total number of observations per category)
        summarize(across(2:3, sum)) %>%
        # merge with dataset; make it last row
        bind_rows(table_descriptive, .) %>%


        # add 999 placeholder number in last row
        mutate(Participant = if_else(is.na(Participant), 999, Participant))

# change row names
row.names(table_descriptive) <-
        c(row.names(table_descriptive)[1:(nrow(table_descriptive) - 1)],
        "total_obs")


table_descriptive %<>%
        # calculate sum of observations per participant (Standart-FB  Missed)
        mutate(row_sum = rowSums(pick(2:3))) %>%

        # calculate compliance to random prompts (Standard-Observations)
        # 42 is the maximum amount of standard observation
                # over the period of 7 days...
                # ...(7 x 6 prompts per day = 42 random prompts)
        mutate(compliance_standard = get("Standard-FB") / 42)


# filter non-compliant participants (compliance <30%)
noncompliant <- table_descriptive %>%
        filter(compliance_standard < 0.3)

# extract column noncompliant; equal to $-formulation
drop <- pull(noncompliant, Participant)

# drop Participants with less than 30% compliance to random prompts
data_compliant <- data_analysis %>%
        filter(!Participant %in% drop)

# descriptive analysis number of observation / compliance
compliant <- table_descriptive %>%
        # remove row 22
        slice(-22) %>%
        # remove rows for which compliance rate is high enough
        filter(compliance_standard >= 0.3)

compliant %<>%

        # get a col cum (total number of observations per category)
        summarize(across(2:4, sum)) %>%
        # merge with dataset; make it last row
        bind_rows(compliant, .) %>%

        # add 999 placeholder number in last row
        mutate(Participant = if_else(is.na(Participant), 999, Participant))

# change row names
row.names(compliant) <-
        c(row.names(compliant)[1:(nrow(compliant) - 1)],
                "total_obs")

mean(compliant$compliance_standard[1:17])

## create overall NA and PA Variables ####

final_data <- data_compliant %>%

        # select only relevant variables
        select(Participant, Form, sad:relaxed,
               socialinteraction_yesno:socialinteraction_scale,
               exercise:selfefficacy_resistance, Diagnose:F44_2) %>%
        # apply row-wise mean etc. from now on
        rowwise() %>%

        # negative affect variable
        mutate(overall_na = mean(c(sad, angry, afraid, guilty,
                ashamed, nervous, lonely, disgustwithself),
                na.rm = TRUE)) %>%

        # positive affect variable
        mutate(overall_pa = mean(c(cheerful, joyful, happy, proud,
                selfconfident, relaxed),
                na.rm = TRUE)) %>%

        ungroup()

# export dataset to excel
write_csv(final_data, "final_data.csv")