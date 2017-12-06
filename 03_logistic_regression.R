# activate needed libraries
library(jsonlite)
library(dplyr)

# Set working directory
setwd("C:/Users/liaos/OneDrive/Documents/Brown/Senior Year/GISP_final")

# In this section, we loop through each of the csv files, and read it in
path = "./data/shots/"
file.names <- dir(path, pattern =".csv")

listOfGames <- list()

for (i in 1:length(file.names)) {
  temp <- read.csv(paste0('data/shots/', file.names[i]))
  
  listOfGames[[i]] <- temp
}

# Combine all the games into one data frame
allShots <- bind_rows(listOfGames)

# Create the logistic regression with output variable shot_outcome
shotMakeMod <<- glm(shot_outcome ~ distance + shooter_height + defender_height + shot_dist + def_angle + LeftOrRight
                  , data = allShots, family = "binomial")

# test it on some test variables to see what the model gives.
test <- data.frame(distance = 2, shooter_height = 170, defender_height = 190, 
                   shot_dist = 60, def_angle = 88, LeftOrRight = 'Right')
predict(shotMakeMod, newdata = test, type = "response")
