#title: Data Script
#description: The primary goal is to create a csv data file inside the code frame
#input: will receive data within the workout01/data folder
#output: will output data into the ../output and create a combined data frame

thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)

curry$name <- "Stephen Curry"
iguodala$name <- "Andre Iguodala"
durant$name <- "Kevin Durant"
green$name <- "Draymond Green"
thompson$name <- "Klay Thompson"

curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"

curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"


curry$minute <- (12 - curry$minutes_remaining) + (12 * (curry$period - 1))
iguodala$minute <- (12 - iguodala$minutes_remaining) + (12 * (iguodala$period - 1))
durant$minute <- (12 - durant$minutes_remaining) + (12 * (durant$period - 1))
green$minute <- (12 - green$minutes_remaining) + (12 * (green$period - 1))
thompson$minute <- (12 - thompson$minutes_remaining) + (12 * (thompson$period - 1))


sink(file= "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink(file= "../output/stephen-curry-summary.txt")
summary(curry)
sink(file= "../output/draymond-green-summary.txt")
summary(green)
sink(file= "../output/kevin-durant-summary.txt")
summary(durant)
sink(file= "../output/klay-thompson-summary.txt")
summary(thompson)


data_combined <- rbind(thompson, curry, durant, green, iguodala)

write.csv(data_combined, file = "../data/shots-data.csv")

sink(file = "../output/shots-data-summary.txt")
summary(data_combined)
sink()

