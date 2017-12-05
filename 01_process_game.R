
# import useful functions
source("00_get_functions.R")
source("Nico_functions.R")

# activate needed libraries
library(jsonlite)
library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(iterators)
library(stringr)
library(lubridate)
library(readr)

# install.packages("devtools")
# devtools::install_github("jimhester/archive")
library(archive)


# function that inserts row(s) at the beginnind of a data frame
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r + 1, nrow(existingDF) + 1), ] <- existingDF[
    seq(r, nrow(existingDF)), ]
  existingDF[r, ] <- newrow
  existingDF
}

# overrides the function from the sourced script, which grabs play-by-play from JSON file 
get_pbp <- function(file){
  the.data.file<-fromJSON(file)
  test <-the.data.file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the.data.file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)
}


sportsVU_path <- "./data/SportsVU_data/"
pbp_path <- "./data/PBP_data/"

allFiles <- list.files(path = sportsVU_path)

# just to change this is enough
current_file <- allFiles[3]


file_name <- archive(paste0(sportsVU_path, current_file))[[1]]
pbp_file <- file_name %>% 
  str_replace(fixed(".json"), "pbp.json")
sportsVU_file <- archive_read(paste0(sportsVU_path, current_file))

# converts the jason into a data frame
all.movements <- sportvu_convert_json(sportsVU_file)
# scrape the play by play data from nba's site
pbp <- get_pbp(paste0(pbp_path, pbp_file))

# filter the ball out to only have players
# filter the events where the ball is above a certain height
df_game <- all.movements %>%
  filter(player_id == "-1") %>% filter(radius > 7) %>%
  #XY distance to the basket
  mutate(threedist = ifelse(
    x_loc < 47,
    {
      sqrt((x_loc - 5.25) ^ 2 + (y_loc - 25) ^ 2)
    }, {
      sqrt((x_loc - 88.75) ^ 2 + (y_loc - 25) ^ 2)
    })) %>%
  #XYZ distance to the basket
  mutate(threedistz = ifelse(
    x_loc < 47,
    {
      sqrt((x_loc - 5.25) ^ 2 + (y_loc - 25) ^ 2 +
             (radius - 10) ^ 2)
    }, {
      sqrt((x_loc - 88.75) ^ 2 +
             (y_loc - 25) ^ 2 + (radius - 10) ^ 2)
    }), total_game_clock = ((720 - game_clock) + (quarter - 1) * 720)) %>% arrange(quarter, desc(game_clock)) %>%
  distinct(total_game_clock, .keep_all = TRUE)

# filter df_game so only stretches where the ball is at least 10 feet in
# the air remain

df_game <- df_game %>% mutate(lag_game_clock = lag(game_clock, n = 1)) %>%
  mutate(event_group = 0)
df_game$event_group[abs(df_game$game_clock - df_game$lag_game_clock) > 0.5] = 1
df_game$event_group_sum <- cumsum(df_game$event_group)
df_game$keep_group = 1

max_event_group <- max(df_game$event_group_sum)
for (i in 0:max_event_group) {
  group <- df_game %>% filter(event_group_sum == i)
  
  # Remove it
  if (max(group$radius) <= 10) {
    df_game$keep_group[df_game$event_group_sum == i] = 0
  }
  
  #if (group$threedist[1] < 20) {
  #  df_game$keep_group[df_game$event_group_sum == i] = 0
 # }
}

df_game <- df_game %>% filter(keep_group == 1)
df_game$keep_group <- NULL
df_game$event_group <- NULL
df_game$event_group_sum <- NULL
df_game$lag_game_clock <- NULL

# find the start and end of plays when ball is in the air using lead and lag functions of dplyr
# where we filter the the differences that are above 1 second
shot_break_end <- df_game %>%
  mutate(lead_game_clock = lead(game_clock, n = 1))
shot_break_end$lead_game_clock[length(shot_break_end$lead_game_clock)] = -1
shot_break_end <- shot_break_end %>%
  filter(abs(game_clock - lead_game_clock) > 1) %>%
  distinct(game_clock, quarter) %>%
  select(game_clock_end = game_clock, quarter)

shot_break_start <- df_game %>%
  mutate(lag_game_clock = lag(game_clock, n = 1)) %>%
  filter(abs(lag_game_clock - game_clock) > 1) %>%
  distinct(game_clock, quarter) %>%
  select(game_clock_start = game_clock, quarter)

##Creates dataframe with start and end times of ball in the air
# we need to match the start and end but from the nature of lean and lag functions
# we need to add a row at the beginning of start and at the end of end
r <- 1
newrow <- c(df_game$game_clock[1], df_game$quarter[1])  # Start with first time
###### length <- nrow(shot_break_start)
###### shot_row <- shot_break_start[length, ]
###### although it does not change the outcome, this line is important to fix a small error in the original code
###### shot_row <- shot_row %>% rename(game_clock_end = game_clock_start)
shot_break_start <- insertRow(shot_break_start, newrow, r)
###### shot_break_end <- bind_rows(shot_break_end, shot_row)  # Add the last time
shot_break <- cbind(shot_break_start, shot_break_end)

##Now that we have the start/end times, lets start by filtering out our dataset to these times
##Also, lets get rid of any plays that are less than 19 feet
##Assign a new id to these plays - shot_id
sumtotal <- NULL
for (i in 1:nrow(shot_break)) {
  df_event <- df_game %>%
    filter(quarter == shot_break$quarter[i] &
             game_clock <= shot_break$game_clock_start[i] &
             game_clock > shot_break$game_clock_end[i]) %>%
    filter(max(threedist) - min(threedist) > 19) %>% mutate(shot_id = i)
  sumtotal <- bind_rows(df_event, sumtotal)
}

##This gives us a dataframe of the ball in air, on plays, where it goes greater than 22 feet

##The next step is matching this data to the play by play data:

##This brings in all the 3 points shots in the play by play data
##This is one way to bring in additional informaton in
# choose the columns that we need and convert their types when needed
pbp_shot <- pbp %>%
  select(EVENTNUM, EVENTMSGTYPE, EVENTMSGACTIONTYPE, HOMEDESCRIPTION,
         VISITORDESCRIPTION, PCTIMESTRING, PERIOD, PLAYER1_ID)
pbp_shot$HOMEDESCRIPTION <- as.character(pbp_shot$HOMEDESCRIPTION)
pbp_shot$VISITORDESCRIPTION <- as.character(pbp_shot$VISITORDESCRIPTION)
# create a threepoint column using the text patterns and filter the data to just have 3 points
pbp_shot$threepoint <- ifelse(
  grepl("3PT", pbp_shot$VISITORDESCRIPTION) |
    grepl("3PT", pbp_shot$HOMEDESCRIPTION), 1, 0)
pbp_shot$block <- ifelse(
  grepl("BLOCK", pbp_shot$VISITORDESCRIPTION) |
    grepl("BLOCK", pbp_shot$HOMEDESCRIPTION), 1, 0)
pbp_shot <- pbp_shot %>% filter(threepoint == 1)
pbp_shot <- pbp_shot %>% filter(block == 0)
pbp_shot$game_clock <- period_to_seconds(
  ms(as.character(pbp_shot$PCTIMESTRING)))
pbp_shot <- pbp_shot %>% filter(game_clock > 2)

# match and merge the data frames
sumtotal3 <- NULL
for (q in 1:4) {
  df_merge <- sumtotal %>% filter(quarter == q)
  df_merge_condensed <- NULL
  events <- unique(df_merge$shot_id)
  for (i in 1:length(events)) {
    df_event <- df_merge[df_merge$shot_id == events[i], ]
    if (min(df_event$threedist) < 3) {
      df_merge_condensed <- bind_rows(df_merge_condensed, df_event)
    }
  }
   df_merge <- df_merge %>% filter(min(threedist) > 3)
   df_merge <- df_merge_condensed
  if (nrow(df_merge) > 0) {
    events <- unique(df_merge$shot_id)
    pbp_q <- pbp_shot %>% filter(PERIOD == q)
    for (i in 1:length(events)) {
      df_merge2 <- df_merge %>% filter(shot_id == events[i])
      merge_time <- min(df_merge2$game_clock)
      timeb <- ifelse(abs(pbp_q$game_clock - merge_time) < 5, 1,
                      0)  # merges if the pbp time is within 5 seconds
      indexc <- match(1, timeb)
      if (Reduce("+", timeb) > 0) {
        df_merge2$EVENTNUM <- as.numeric(as.character(pbp_q$EVENTNUM[indexc]))
        df_merge2$EVENTMSGTYPE <- as.numeric(as.character(pbp_q$EVENTMSGTYPE[indexc]))
        df_merge2$PLAYER1_ID <- as.numeric(as.character(pbp_q$PLAYER1_ID[indexc]))
      } else {
        df_merge2$EVENTNUM <- 999  # 999 indicates no match
        df_merge2$EVENTMSGTYPE <- 999
        df_merge2$PLAYER1_ID <- 999
      }
      sumtotal3 <- bind_rows(df_merge2, sumtotal3)
    }
  }
}
sumtotal3 <- sumtotal3 %>% filter(EVENTMSGTYPE != '999')  # Remove any no
# match plays

##Now we have a dataframe of 3 point plays from when the ball leaves the shooters hand to when it reaches the basket

##Finds the point where the ball leaves the shooters hand
df_startshot <- sumtotal3 %>%
  group_by(shot_id) %>% filter(row_number() == 1) %>% ungroup() %>%
  select(shot_id, EVENTMSGTYPE, game_clock, quarter, PLAYER1_ID,
         shot_clock) %>% arrange(quarter, desc(game_clock))

df_total <- NULL
##loops through each three point play
for (i in 1:nrow(df_startshot)) {
  
  ##Get start of the play
  df_startplay <- all.movements %>%
    filter(quarter == df_startshot$quarter[i] &
             game_clock >= df_startshot$game_clock[i]) %>%
    filter(player_id == "-1") %>%
    distinct(quarter, game_clock, .keep_all = TRUE) %>%
    arrange(quarter, game_clock) %>% filter(!is.na(shot_clock)) %>%
    mutate(lead_shot_clock = lead(shot_clock, n = 1)) %>%
    filter(shot_clock - lead_shot_clock > 1) %>% head(1)
  ##Get the ball/player data now that we have the start/end time
  if (nrow(df_startplay) > 0) {
    ##Subset down to just data for this play based on length of play
    df_play <- all.movements %>%
      filter(quarter == df_startshot$quarter[i] &
               game_clock <= (df_startplay$game_clock) &
               game_clock >= df_startshot$game_clock[i]) %>%
      # df_play <- all.movements %>% filter (quarter==df_startshot$quarter[i] & game_clock <= (df_startshot$game_clock[i]+length_of_play) & game_clock >= df_startshot$game_clock[i]) %>%
      mutate(playid = i) %>%
      distinct(player_id, quarter, game_clock, .keep_all = TRUE) %>%
      arrange(desc(game_clock), player_id)
    #Rotate plays depending upon location of the shot
    if (tail(df_play$x_loc, 1) > 47) {
      df_play <- df_play %>%
        mutate(x_loc = 94 - x_loc) %>% mutate(y_loc = 50 - y_loc)
    }
    #df_play$gameid <- file_name %>% str_replace(fixed(".json"), "")
    df_play$gameid <- 0021500493
    df_play$EVENTMSGTYPE <- df_startshot$EVENTMSGTYPE[i]  # Adding in some
    # of the pbp
    # data
    df_play$PLAYER1_ID <- df_startshot$PLAYER1_ID[i]
    df_total <- bind_rows(df_total, df_play)
  }
}


df_startshot$real_clock <- 720 * (4 - df_startshot$quarter) + df_startshot$game_clock

df_total$real_clock <- 720 * (4 - df_total$quarter) + df_total$game_clock

df_1 <- df_startshot %>%
  select(real_clock, quarter, game_clock, shot_clock, PLAYER1_ID) %>%
  rename(player_id = PLAYER1_ID)

df_total$player_id <- as.numeric(df_total$player_id)
df_total <- rename(df_total, game_id = gameid)

df_2 <- df_total %>%
  select(player_id, lastname, firstname, team_id, event.id, real_clock, game_id )

final_df <- df_1 %>%
  left_join(df_2, by = c("real_clock", "player_id"))

real <- unique(final_df) %>%
  rename(match_clock = real_clock, 
         quarter_clock = game_clock,
         shooter_id = player_id,
         shooter_lastname = lastname, 
         shooter_firstname = firstname,
         shooter_team_id = team_id,
         event_id = event.id,
         match_id = game_id) %>%
  select(match_id,
         match_clock,
         quarter,
         quarter_clock,
         shot_clock,
         shooter_id,
         shooter_lastname, 
         shooter_firstname,
         shooter_team_id,
         event_id)

player_position <- function(eventid,gameclock){
  ##Returns positions of all players at a time
  ##Requires data in total and balltime
  dfall <- all.movements %>% filter(game_clock == gameclock,event.id==eventid)  %>% 
      filter(lastname!="ball") %>% select (player_id, lastname, firstname, team_id, x_loc, y_loc)
  colnames(dfall) <- c('defender_id','defender_lastname','defender_firstname', "defender_team_id", "X", "Y")
  return(dfall)
}


find_defence_dist <- function(df) {
  defence <- data.frame(shooter_X = NA, shooter_Y = NA, defender_id = NA, defender_lastname = NA, defender_firstname = NA, defender_team_id = NA, defender_X = NA, defender_Y = NA, distance = NA)
  for(i in 1:nrow(df)) {
    mat <- player_position(df$event_id[i], df$quarter_clock[i])
    shooter <- mat %>% filter(defender_id == df$shooter_id[i])
    
    x1 <- shooter[1, 5]
    y1 <- shooter[1, 6]
    
    x2 <- replicate(10, NA)
    y2 <- replicate(10, NA)
    
    for(j in 1:10) {
      x2[j] <- mat[j, 5]
      y2[j] <- mat[j, 6]
    }
    
    d <- replicate(10, NA)
    
    for(k in 1:10) {
      d[k] <- dist(rbind(c(x1, y1), c(x2[k], y2[k])))
    }
  

    min_dist <- 999

    for(l in 1:length(d)) {
      if(d[l] != 0 & shooter[1, 4] != mat[l, 4] & d[l] < min_dist) {
        min_dist <- d[l]
      }
    }
    
    index <- match(min_dist, d)
    df_temp <- data.frame(shooter_X = shooter[1, 5], shooter_Y = shooter[1, 6], defender_id = mat[index, 1], defender_lastname = mat[index, 2], defender_firstname = mat[index, 3], defender_team_id = mat[index, 4], defender_X = mat[index, 5], defender_Y = mat[index, 6], distance = min_dist)
    defence <- bind_rows(defence, df_temp)
  }
  defence <- defence[-1, ]
  return(defence)
}

def <- find_defence_dist(real)

haha <- bind_cols(real, def)

shot_outcome <- replicate(nrow(df_startshot), NA)
for(i in 1:nrow(df_startshot)) {
  shot_outcome[i] <- ifelse(df_startshot$EVENTMSGTYPE[i] == 1, 1, 0)
}

haha$shot_outcome <- shot_outcome

## function to include height info
##
##
get_height <- function(player, heights) {
  for(i in 1:length(heights$player)) {
    if(heights$player[i] == player) break
  }
  if(heights$player[i] != player) return(NA)
  return(heights$height[i])
}

heights <- read.csv("data/player_height.csv")

defender_height <- replicate(nrow(haha), NA)
shooter_height <- replicate(nrow(haha), NA)
for(i in 1:nrow(haha)) {
  defender_name <- paste(haha$defender_firstname[i], haha$defender_lastname[i], sep = " ")
  shooter_name <- paste(haha$shooter_firstname[i], haha$shooter_lastname[i], sep = " ")
  defender_height[i] <- get_height(defender_name, heights)
  shooter_height[i] <- get_height(shooter_name, heights)
}

height_difference <- shooter_height - defender_height

haha$shooter_height <- shooter_height
haha$defender_height <- defender_height
haha$height_difference <- height_difference

GSW_files = list.files(path = sportsVU_path, pattern = "*.GSW.*")
