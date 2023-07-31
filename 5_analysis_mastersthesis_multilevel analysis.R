library(misty)
library(tidyverse)

file_name <- "final_data.csv"

# import data
final_data <- read_csv(file_name)

# remove unnecessary objects
rm(file_name)


# multilevel correlation overall affect, social interaction and exercise
multilevel.cor(
        final_data[, c(
                "overall_na",
                "overall_pa",
                "socialinteraction_yesno",
                "exercise"
        )],
        cluster = final_data$Participant,
        sig = TRUE,
        alpha = 0.05,
        p.adj = "bonferroni",
        print = "all",
        write = "cor_results1"
)

# less observations available for social interaction!
multilevel.cor(
        final_data[, c(
                "overall_na",
                "overall_pa",
                "socialinteraction_scale",
                "exercise"
        )],
        cluster = final_data$Participant,
        sig = TRUE,
        alpha = 0.05,
        p.adj = "bonferroni",
        print = "all",
        write = "cor_results2"
)


# multilevel analysis for specific NA variables
multilevel.cor(
        final_data[, c(
                "sad",
                "angry",
                "afraid",
                "guilty",
                "ashamed",
                "nervous",
                "lonely",
                "disgustwithself",
                "exercise"
        )],
        cluster = final_data$Participant,
        sig = TRUE,
        alpha = 0.05,
        p.adj = "bonferroni",
        print = "all",
        write = "cor_results3"
)

# multilevel analysis for specific PA variables
multilevel.cor(
        final_data[, c(
                "cheerful",
                "joyful",
                "happy",
                "proud",
                "selfconfident",
                "relaxed",
                "exercise"
        )],
        cluster = final_data$Participant,
        sig = TRUE,
        alpha = 0.05,
        p.adj = "bonferroni",
        print = "all",
        write = "cor_results4"
)

# associations of exercise_person with social interaction
multilevel.cor(
        final_data[, c(
                "overall_na",
                "overall_pa",
                "exercise_person",
                "socialinteraction_scale"
        )],
        cluster = final_data$Participant,
        sig = TRUE,
        alpha = 0.05,
        p.adj = "bonferroni",
        print = "all",
        write = "cor_results5"
)