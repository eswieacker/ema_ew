library(tidyverse)
library(haven)
library(lavaan)
library(magrittr)
library(glue)

file_name <- "final_data.csv"

# import data
final_data <- read_csv(file_name)

# remove unnecessary objects
rm(file_name)

## descriptive statistics exercise_person

h_exercise_person_jg <- final_data %>%
        # tabulate count of var
        count(exercise_person) %>%

        # remove large number of NA values
        filter(!(is.na(exercise_person))) %>%

        # sort largest to smallest
        arrange(desc(n)) %>%

        # create rounded relative share variable
        mutate(prop = round(n / sum(n, na.rm = TRUE), 2))


h_na <- final_data %>%
        count(overall_na) %>%
        arrange(desc(n))

h_pa <- final_data %>%
        count(overall_pa) %>%
        arrange(desc(n))



ggplot(final_data, aes(x = exercise_person)) +
        geom_bar(
                aes(
                        y = ..prop..,
                        fill = exercise_person
                ),
                position = "dodge",
                color = "black"
        ) +
        labs(x = "Who did you do exercise with?", y = "percentage")


ggplot(final_data, aes(x = exercise_person)) +
        geom_bar(
                aes(
                        y = ..prop..,
                        fill = exercise_person
                ),
                position = "dodge",
                color = "black"
        ) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Who did you do exercise with?", y = "percentage")

# Descriptive Statistics overall NA & PA
H_NA <- table(final_data$overall_NA)
print(H_NA)
plot(H_NA, col = "red")

H_PA <- table(final_data$overall_PA)
plot(H_PA, col = "dark green")

# ggplot2

  # NA
ggplot(data = final_data, aes(x = overall_NA)) +
        geom_bar(color = "dark red", fill = "dark red") +
        labs(x = "negativer Affekt", y = " ") +
        theme(
                panel.background = element_rect(fill = "white"),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black")
        )

# PA
ggplot(data = final_data, aes(x = overall_PA)) +
        geom_bar(color = "darkgreen", fill = "darkgreen") +
        labs(x = "positiver Affekt", y = " ") +
        theme(
                panel.background = element_rect(fill = "white"),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black")
        )
