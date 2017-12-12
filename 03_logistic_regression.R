# activate needed libraries
library(dplyr)
library(broom)
library(ggplot2)
library(ROCR)
library(MASS)

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

steph$trueAngle <- steph$def_angle
steph$trueAngle[steph$LeftOrRight == "Left"] = -1*steph$trueAngle[steph$LeftOrRight == "Left"]
steph <- na.omit(steph)

# Create the logistic regression with output variable shot_outcome
shotMakeMod <<- glm(shot_outcome ~ shot_clock + quarter + quarter_clock + shooter_X + shooter_Y
                    + distance + shooter_height + defender_height + shot_dist + def_angle + LeftOrRight
                  ,data = steph, family = "binomial")

# test it on some test variables to see what the model gives.
test <- data.frame(distance = 0.5, shooter_height = 170, defender_height = 170, 
                   shot_dist = 23, trueAngle = -23)#def_angle = 5, LeftOrRight = 'Left')
predict(shotMakeMod, newdata = test, type = "response")

# Model Testing Stuff
tidy(shotMakeMod, exponentiate = T)#, conf.int = T)

# Hoslem Test
hoslem.test(steph$shot_outcome, fitted(shotMakeMod), g = 10)

# AUC Test
prob <- predict(aic_shotMakeMod)
pred <- prediction(prob, steph$shot_outcome)
perf <- performance(pred, 'tpr', 'fpr')

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) + geom_abline(intercept = 0, slope = 1,
                                       colour = "blue")+
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

# Stepwise AIC Model
aic_shotMakeMod <- stepAIC(shotMakeMod, direction="backward", k=2)
