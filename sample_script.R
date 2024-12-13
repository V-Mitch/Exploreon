library(fitdistrplus)
source('Exploreon_class.R')

file_path <- "C:/Users/victo/OneDrive/Spectre/R tests/VitaminA.csv"
data <- read.csv(file_path, sep = ';')
file_path <- "C:/Users/victo/OneDrive/Spectre/R tests/Cardiotocographic.csv"
data <- read.csv(file_path)

# Exploreon$class_methods$get_summary_stats(data)

explore <- Exploreon$new(data)
explore$get_L0_stats()
explore$get_L1_stats()


Exploreon$new(data)$get_summary_stats()