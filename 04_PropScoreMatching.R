library(dplyr)
library(MatchIt)

library(png)
library(plotrix)

library(ggplot2)

# For reproducibility
set.seed(95)

# import dataset
steph <- read.csv("steph.csv")

# remove extra columns 
steph <- steph[ , c(-1, -28)]

# change the name of distance since matchit also creates a column of distance
names(steph)[20] <- "player_distance"

# matchit can not handle NAs
steph <- na.omit(steph)

# see how many laft and right shots available
sum(steph$LeftOrRight == "Left")
sum(steph$LeftOrRight == "Right")

# convert LeftOrRight to binary style, making left "1", aka treatment group
steph$LeftOrRight <- as.character(steph$LeftOrRight)
for(i in 1:nrow(steph)) {
  if(steph$LeftOrRight[i] == "Left") {
    steph$LeftOrRight[i] <- 1
  } else {
    steph$LeftOrRight[i] <- 0
  }
}

# store this column as factor
steph$LeftOrRight <- as.factor(steph$LeftOrRight)

# check the structure
str(steph$LeftOrRight)


## Logistic Regression
# note that some columns are collinear such as defence heigh and height difference. 
# no need to include these variables
fit_shot <- glm(shot_outcome ~ 
             quarter + 
             quarter_clock +
             shot_clock +
             shooter_X +
             shooter_Y +
             defender_X +
             defender_Y +
             player_distance +
             defender_height +
             shot_dist +
             def_angle +
             LeftOrRight +
             def_reyting,
             #def_position, 
           data = steph, 
           family="binomial")

# summary
# right or left looks interesting
summary(fit_shot)

# propensity score matching
# we can do it by matching the nearest pairs or by manually defining a caliper
# nearest method does not give very good matching, putting caliper decreases the number of matches
# caliper method still gives a less but still significant p-value in paired t-test and mcnemar's test
m.out <- matchit(LeftOrRight ~ 
         #           quarter + 
         #           quarter_clock +
                    shot_clock +
                    shooter_X +
                    shooter_Y +
          #          defender_X +
          #          defender_Y +
                    player_distance +
                    defender_height +
                    shot_dist +
                    def_angle +
                    def_reyting,
                    #def_position, 
                  data = steph, 
           #       method = "nearest",
                 caliper = 0.15
)

m.out
# summary
summary(m.out)

# plot to see the quality of matching
plot(m.out, type = "jitter", interactive = F)
plot(m.out, type = "hist")

# extract the matched pairs
match_data <- match.data(m.out)

# paired t test for variables to check the matching quality
t.test(match_data$shot_clock ~ match_data$LeftOrRight, paired = T)
t.test(match_data$shooter_X ~ match_data$LeftOrRight, paired = T)
t.test(match_data$shooter_Y ~ match_data$LeftOrRight, paired = T)
t.test(match_data$player_distance ~ match_data$LeftOrRight, paired = T)
t.test(match_data$defender_height ~ match_data$LeftOrRight, paired = T)
t.test(match_data$shot_dist ~ match_data$LeftOrRight, paired = T)
t.test(match_data$def_angle ~ match_data$LeftOrRight, paired = T)
t.test(match_data$def_reyting ~ match_data$LeftOrRight, paired = T)

# save as a df
matches	<- data.frame(m.out$match.matrix)

# find	the	matches for	one	for	group	1	and	group	2
group1	<- match(row.names(matches),	row.names(match_data))
group2	<- match(matches$X1,	row.names(match_data))

#	extract	the	outcome	value	for	the	matches (Right or Left)
yT		<- match_data$shot_outcome[group1]
yC		<- match_data$shot_outcome[group2]

propST <- match_data$distance[group1]
propSC <- match_data$distance[group2]

# bind
matched_cases	<- cbind(matches,	group1, group2, yT,	yC, propST, propSC)
matched_cases <- na.omit(matched_cases)

# create table for mcnemar's test
tbl <- table(matched_cases$yT, matched_cases$yC)
tbl

# test for paired categorical data
mcnemar.test(tbl)

# Paired	t-test for percentage
#t.test(matched_cases$yT,	matched_cases$yC,	paired	= TRUE, alternative = "two.sided")

# Paired	t-test for percentage
#t.test(matched_cases$yT,	matched_cases$yC,	paired	= TRUE, alternative = "greater")

# We can play around with the caliper to find a good value
# We can conduct paired t or mcnemar's test on every variables that we are matching for 
# to see the final difference is significant or not

# Returns a graph given a row in matched_cases
# mc is matched_cases
# md is match_data
# rightOrLeft is 1 for right (group 1), 0 for left (group 2)
getGraph <- function(mc, md, row, rightOrLeft) {
  
  if (rightOrLeft) {
    rowToPlot <- mc$group1[row]
  } else {
    rowToPlot <- mc$group2[row]
  }
  
  data <- md[rowToPlot,]
  
  # We plot an empty graph (type = 'n') with dimensions 50x94
  
  
  # Width and height are given in pixels
  # Make sure to specify this!!!
  
  # windows(width = 5, height = 9.4)
  plot(0:94, xlim=c(0,50), ylim=c(94, 0), type="n")
  # par grabs the parameters from the current plot
  lim <- par()
  # rasterImage just slaps an image on top of the plot. It's is a dependency of the plotrix library
  rasterImage(the.court, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  
  points(data$shooter_Y, data$shooter_X, type = "p", pch = 19, col = "green", cex = 10)
  points(data$defender_Y, data$defender_X, type = "p", pch = 'x', col = "black", cex = 10)
  
}


# Returns:
# Shooter name, Defender Name,
# Defender Height, Defender Distance, Defender Angle, Defender Pass. Rating
# MakeOrMiss

# mc is matched_cases
# md is match_data
# rightOrLeft is 1 for right (group 1), 0 for left (group 2)

getInfo <- function(mc, md, row, rightOrLeft) {
  
  if (rightOrLeft) {
    rowToPlot <- mc$group1[row]
  } else {
    rowToPlot <- mc$group2[row]
  }
  
  data <- md[rowToPlot,]
  
  ## data is the row that contains all the information. Format however 
  ## you wish
}

for (i in 1:146) {
  name <- paste0(as.character(i), "L.png")
  ggsave(name, getGraph(matched_cases, match_data, i, 1), path = "./www/", width = 18.25, height = 35.25)
}

for (i in 1:146) {
  name <- paste0(as.character(i), "R.png")
  ggsave(name, getGraph(matched_cases, match_data, i, 0), path = "./www/", width = 18.25, height = 35.25)
}
  
ggsave("1L.png", getGraph(matched_cases, match_data, 1, 1), width = 18.25, height = 35.25)
