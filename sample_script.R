library(fitdistrplus)
source('v1_test.R')

file_path <- "C:/Users/victo/OneDrive/Spectre/R tests/VitaminA.csv"
data <- read.csv(file_path, sep = ';')

# Exploreon$class_methods$get_summary_stats(data)

explore <- Exploreon$new(data)
explore$get_data_stats()
explore$get_summary_stats()


Exploreon$new(data)$get_summary_stats()