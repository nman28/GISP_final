# activate needed libraries
library(dplyr)

# Set working directory
setwd("C:/Users/liaos/OneDrive/Documents/Brown/Senior Year/GISP_final")

# In this section, we loop through each of the csv files, and read it in
pathWarriors = "./data/shots/"
pathRockets = "./data/HOU_shots/"

teamThrees <- function(path){
  file.names <- dir(path, pattern =".csv")
  listOfGames <- list()

  for (i in 1:length(file.names)) {
    temp <- read.csv(paste0('data/shots/', file.names[i]))
    
    listOfGames[[i]] <- temp
  }
  
  
  # Combine all the games into one data frame
  teamShots <- bind_rows(listOfGames)
}

warriors <- teamThrees(pathWarriors)
rockets <- teamThrees(pathRockets)


# Create the logistic regression with output variable shot_outcome
shotMakeMod <<- glm(shot_outcome ~ distance + shooter_height + defender_height + shot_dist + def_angle + LeftOrRight
                  , data = allShots, family = "binomial")
# test it on some test variables to see what the model gives.
test <- data.frame(distance = 5, shooter_height = 170, defender_height = 170, 
                   shot_dist = 23, def_angle = 20, LeftOrRight = 'Left')
predict(shotMakeMod, newdata = test, type = "response")
