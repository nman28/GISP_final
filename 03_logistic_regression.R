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
    temp <- read.csv(paste0(substring(path, 3), file.names[i]))
    
    listOfGames[[i]] <- temp
  }

  # Combine all the games into one data frame
  teamShots <- bind_rows(listOfGames)
}

warriors <- teamThrees(pathWarriors)
rockets <- teamThrees(pathRockets)

warriorsMissing <- read.csv("data/missingHeightsWarriors.csv")
warriorsMissing <- warriorsMissing[,c(-1)]
warriorsNotMissing <- warriors[!(is.na(warriors$height_difference)),]

warriors <- rbind(warriorsMissing, warriorsNotMissing)
warriors$height_difference <- warriors$shooter_height - warriors$defender_height
steph <- warriors[warriors$shooter_firstname == 'Stephen',]
steph <- steph[steph$shot_dist > 21,]

# Create the logistic regression with output variable shot_outcome
shotMakeMod <<- glm(shot_outcome ~ distance + shooter_height + defender_height + shot_dist + def_angle + LeftOrRight
                  ,data = steph, family = "binomial")

# test it on some test variables to see what the model gives.
test <- data.frame(distance = 0.5, shooter_height = 170, defender_height = 170, 
                   shot_dist = 23, def_angle = 40, LeftOrRight = 'Left')
predict(shotMakeMod, newdata = test, type = "response")

tidy(shotMakeMod, exponentiate = T, conf.int = T)
