library(tidyverse)
library(haven)
library(lavaan)
library(magrittr)
library(glue)
library(extrafont)

file_name <- "final_data.csv"

# import data
final_data <- read_csv(file_name)

# remove unnecessary objects
rm(file_name)


## new subset of variables describing driven exercise ####

data_driven_exercise <- final_data %>%
  select(Participant, exercise:selfefficacy_resistance)

# number of times exercise
sum(data_driven_exercise$exercise, na.rm = TRUE)

# one time a participant mistakenly said she was exercising
print(data_driven_exercise[486, "exercise_category"])

# clear following ratings (change to NA)
data_driven_exercise %<>%

  # set these columns to NA for row 617
  mutate(across(exercise_category:selfefficacy_resistance,
                ~ if_else(row_number() == 617, as(NA, class(.x)), .x)),

         # set this column to 0 for row 617
         exercise = if_else(row_number() == 617, 0, exercise))

# number of times exercise
sum(data_driven_exercise$exercise, na.rm = TRUE)

# descriptive statistics duration exercise

exercise_duration <- data_driven_exercise %>%

  # apply summary statistics to col exercise_minutes
  summarize(across(exercise_minutes,
                   list(mean_duration = ~round(mean(., na.rm = TRUE), 2),
                        min_duration = ~round(min(., na.rm = TRUE), 2),
                        max_duration = ~round(max(., na.rm = TRUE), 2),
                        sd = ~round(sd(., na.rm = TRUE), 2)),
                   # set col names to mean_duration, min_duration etc.
                   .names = "{.fn}"))

## descriptive statistics categories of exercise ####

# Spazieren
spazieren_strings <- c("Spaziergang",
                       "Spazierengehen",
                       "Spazieren gehen",
                       "Gegangen",
                       "Spazieren gegangen",
                       "Spaziergang zum See und zurück",
                       "Zum See gegangen",
                       "Spaziergang zur Bank, hingesetzt, geredet, zurück gegangen", #nolint
                       "Spaziergang zum Hafen, hinsetzen, lesen, zurück",
                       "Spazierem",
                       "Spazeirengehen",
                       "Mit meiner mama und mit unserem Hund spazieren gegangen", #nolint
                       "Langsames Spazierengehen",
                       "Ich bin zur Eisdiele gelaufen, da wir als Zwischenmahlzeit Eis gegessen haben", #nolint
                       "Ich bin spazieren gegangen")

spazieren_schwimmen_strings <- c("Spazieren, schwimmen(5min)",
                                 " Spazieren, schwimmen (5min)",
                                 "Schwimmen, spaziere")

# replace strings; harmonize them
data_driven_exercise %<>%
        mutate(exercise_category = case_when(
                exercise_category %in% c(spazieren_strings) ~
                        "Spazieren",
                exercise_category %in% c(spazieren_schwimmen_strings) ~
                        "Spazieren, schwimmen",
                # keep as is if the strings do not match
                .default = exercise_category))

# remove unnecessary objects
rm(spazieren_schwimmen_strings, spazieren_strings)


# descriptive stats exercise categories
categories_exercise <- data_driven_exercise %>%

        # tabulate exercise_category, sort largest to smallest; include NA
        filter(exercise == 1) %>%
        count(exercise_category, sort = TRUE) %>%

        # calculate relative frequencies
        mutate(relh = round(n / sum(n) * 100, 0))

## descriptive statistics reasons for exercise ####

reasons_mean_sd <- data_driven_exercise %>%
        # apply column means and sd to all reasons; rm NA values
        summarize(across(calories:selfefficacy_resistance,
                        list(mean = ~ mean(.x, na.rm = TRUE),
                        sd = ~ sd(.x, na.rm = TRUE)),
                        # name columns in a format suitable for pivoting
                        .names = "{.col}!{.fn}")) %>%

  # bring into vertical format
        pivot_longer(cols = everything(),
                # create two three cols: category, mean, sd
                # the latter two are ".value"
                names_to = c("category", ".value"),
                # refers to how I named the columns in summarize()
                names_sep = "!") %>%

        # sort by mean from largest to smallest
        arrange(desc(mean))

# export to csv
write_csv(reasons_mean_sd, "reasons_mean_sd.csv")

# Plot reasons

## remove selfefficacy and change names

reasons_mean_sd_plot <- reasons_mean_sd %>%
        filter(category != "selfefficacy_resistance") %>%
        mutate(category = str_replace_all(category, pattern = c(
                "goals" = "reach goals",
                "positive_emotions" = "positive emotions",
                "body" = "feel good in my body",
                "occupied_meaningfulness" = "occupied meaningfulness",
                "emotionregulation" = "emotion regulation",
                "thoughts" = "escape negative thoughts",
                "control" = "feel in control",
                "calories" = "burn calories",
                "meal" = "allow meal",
                "occupied_loneliness" = "occupied loneliness",
                "tofeel" = "feel myself",
                "punish" = "punishment")))


# font_import()
loadfonts(device = "win")

ggplot(reasons_mean_sd_plot) +
        geom_col(aes(
                x = forcats::fct_reorder(category, mean, .desc = TRUE),
                y = mean
        ), width = 0.7, fill = "grey") +
        labs(
                x = "Reasons for Exercise", y = "Mean",
                title = "Self-Reports of Reasons for Exercise"#,
                #subtitle = "this is a potential subtitle"
        ) +
        theme_minimal(base_family = "Times New Roman") +
        theme(
                title = element_text(size = 14),

                axis.title = element_text(size = 14, face = "bold"),
                axis.title.x = element_text(margin = margin(t = 20)),
                axis.title.y = element_text(margin = margin(r = 10)),

                axis.text = element_text(size = 12, colour = "black"),
                axis.text.x = element_text(angle = 45, hjust = 1,
                        vjust = 0.9, margin = margin(t = -20)),

                panel.grid.minor.y = element_line(colour = "white"),
                panel.grid.major.x = element_line(colour = "white"),
                panel.grid.minor.x = element_line(colour = "white")
        )


ggsave("Figure1.jpeg", dpi = 320, width = 15, height = 15, units = "cm")