# activate needed libraries
library(jsonlite)
library(dplyr)

setwd("C:/Users/liaos/OneDrive/Documents/Brown/Senior Year/GISP_final")

path = "./data/shots/"
file.names <- dir(path, pattern =".csv")

listOfGames <- list()

for (i in 1:length(file.names)) {
  temp <- read.csv(paste0('data/shots/', file.names[i]))
  
  listOfGames[[i]] <- temp
}

allShots <- bind_rows(listOfGames)

shotMakeMod <<- glm(shot_outcome ~ distance + shooter_height + defender_height + shot_dist + def_angle + LeftOrRight
                  , data = allShots, family = "binomial")
