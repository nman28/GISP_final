
library(dplyr)
library(MatchIt)

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
             def_reyting +
             def_position, 
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
                    def_reyting +
                    def_position, 
                  data = steph, 
           #       method = "nearest",
                 caliper = 0.3)

# summary
summary(m.out)

# plot to see the quality of matching
plot(m.out, type = "jitter", interactive = F)
plot(m.out, type = "hist")

# extract the matched pairs
match_data <- match.data(m.out)

# save as a df
matches	<- data.frame(m.out$match.matrix)

# find	the	matches for	one	for	group	1	and	group	2
group1	<- match(row.names(matches),	row.names(match_data))
group2	<- match(matches$X1,	row.names(match_data))

#	extract	the	outcome	value	for	the	matches (Right or Left)
yT		<- match_data$shot_outcome[group1]
yC		<- match_data$shot_outcome[group2]

# bind
matched_cases	<- cbind(matches,	yT,	yC)

# create table for mcnemar's test
tbl <- table(matched_cases$yT, matched_cases$yC)
tbl

# test for paired categorical data
mcnemar.test(tbl)

# Paired	t-test for percentage
t.test(matched_cases$yT,	matched_cases$yC,	paired	= TRUE, alternative = "two.sided")

# We can play around with the caliper to find a good value
# We can conduct paired t or mcnemar's test on every variables that we are matching for 
# to see the final difference is significant or not



