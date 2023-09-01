#Redoing code
options(scipen=9999)
#setting adequate working directory
setwd("~/SMT Data Challenge")
library(tidyverse)
library(gganimate)
#reading in csv of game ids to identify each game file
game_ids<-read_csv("game_ids.csv")
#function to read through each game file and return the desired data
clean_throw_data <- function(file_index){  #printing filename for sake of tracking
  print(paste("File index is ", file_index, sep=""))
  #name of file based on game
  game_id <- unlist(game_ids[file_index,])
  #reading in various files for each game
  ball_filename <- paste("ball_pos-", game_id, ".csv", sep="")
  ball_pos <- read_csv(ball_filename)
  ball_pos[1] <- NULL
  game_events_filename <- paste("game_events-", game_id, ".csv", sep="")
  game_events <- read_csv(game_events_filename)
  game_events[1] <- NULL
  game_info_filename <- paste("game_info-", game_id, ".csv", sep="")
  game_info <- read_csv(game_info_filename)
  game_info[1] <- NULL
  player_pos_filename <- paste("player_pos-", game_id, ".csv", sep="")
  player_pos <- read_csv(player_pos_filename)
  player_pos[1] <- NULL
  #binding player positioning and ball positioning into one
  all_positions<-rbind(ball_pos %>%
                         reframe(
                           game_str, 
                           play_id,
                           timestamp,
                           field_x=ball_position_x, 
                           field_y=ball_position_y,
                           field_z=ball_position_z,
                           player_position="ball"),
                       player_pos %>%
                         reframe(
                           game_str, 
                           play_id,
                           timestamp,
                           field_x, 
                           field_y,
                           field_z=-999,
                           player_position))
  #merging all of the data together so everything is IDed
  all_positions<-merge(all_positions, game_events, by=c("game_str", "play_id", "timestamp", "player_position"), all.x=T)
  all_positions <- merge(all_positions, game_info, by=c("game_str", "play_per_game", "at_bat"), all.x=T)
  #filtering to retain only all throw ids
  throw_ids <- all_positions %>%
    filter(event_code==3) %>%
    reframe(play_id, player_position, timestamp)
  #creating empty data frame to fill with for loop
  throw_data <- data.frame()
  for (playid in 1:nrow(throw_ids)) {
    #printing play id for sake of tracking
    print(playid)
    #identifying each tracked event, eg catch, throw, bip, etc. and ordering them
    events<-all_positions %>%
      filter(play_id == throw_ids[playid, 1], !is.na(event_code)) %>%
      arrange(timestamp) %>%
      #creating new column to list order of events for sake of reference
      mutate(ordered_id = 1:nrow(all_positions %>%
                                   filter(play_id == throw_ids[playid, 1], !is.na(event_code))))
    #getting the proper data for just the throw
    new_throw_data <- events %>%
      #referencing the third and second columns in the throw_ids dataframe
      #which pulls the correct data given the timestamp and player_position in the throw_ids frame
      filter(player_position==throw_ids[playid, 2],
             timestamp==throw_ids[playid, 3])  %>%
      #summarising with desired variables
      reframe(timestamp, 
              event_code, 
              player_position, 
              ordered_id,
              at_bat)
    #getting the specific throw id for sake of reference
    throw_id <- unlist(new_throw_data$ordered_id)
    #getting data for the following play, based on the id of the throw
    following_play <- events %>%
      filter(ordered_id == throw_id + 1) %>%
      #summarising desired variables
      reframe(next_timestamp=timestamp, 
              next_event_code=event_code, 
              next_player_position=player_position, 
              next_id=ordered_id)
    # Create an empty dataframe with the specified column names
    following_play_dummy <-data.frame(
      next_timestamp = "NO event AFTER",
      next_event_code = "NO event AFTER",
      next_player_position = "NO event AFTER",
      next_id = "NO event AFTER"
    )
    #if there is no following play, return the dummy
    if (nrow(following_play)==0){
      following_play <- following_play_dummy
    }
    #binding the throw data together
    throw_data<-rbind(throw_data, 
                      #new data incorporated
                      data.frame(play_id=throw_ids[playid, 1] , new_throw_data, following_play))
  }
  #getting the position of the ball at the time of the throw and catch
  ball_positions <- all_positions %>%
    filter(player_position=="ball") %>%
    reframe(timestamp, play_id, field_x, field_y, field_z)
  next_frame_ball_positions <- all_positions %>%
    arrange(timestamp) %>%
    filter(player_position=="ball") %>%
    #timestamp1 is the actual timestamp of the play
    reframe(timestamp1=timestamp, timestamp=lag(timestamp, 15), play_id, field_x, field_y, field_z) 
  next_ball_positions <- all_positions %>%
    filter(player_position=="ball") %>%
    reframe(timestamp, play_id, field_x, field_y, field_z)
  #setting names of next  ball positions to be distinctive
  names(next_frame_ball_positions) <- paste("next_frame", names(next_frame_ball_positions), sep="_")
  names(next_ball_positions) <- paste("next", names(next_ball_positions), sep="_")
  #merging ball positions together based on time of throw
  throw_data<-merge(throw_data, ball_positions, by=c("timestamp", "play_id"), all.x=T)
  #merging next play based on id addition to get time of catch/ballpickup/any conclusion to throw
  throw_data<-merge(throw_data, next_ball_positions, 
                    by.x=c("next_timestamp", "play_id"),
                    by.y=c("next_timestamp","next_play_id"), all.x=T)
  throw_data<-merge(throw_data, next_frame_ball_positions, 
                    by.x=c("timestamp", "play_id"),
                    by.y=c("next_frame_timestamp","next_frame_play_id"), all.x=T)
  #getting data to return game state
  game_data <- all_positions %>%
    filter(!is.na(play_per_game), !duplicated(at_bat)) 
  #at bat column, and game state columns
  game_data <- game_data[c(3,15:27)]
  #next play, at bat is used to string the two back to back plays together
  new_game_data <- game_data %>%
    mutate(at_bat=at_bat-1)
  #setting names of next game states to be distinctive
  names(new_game_data) <- paste("next", names(new_game_data), sep="_")
  #merging game state data with the big throw data frame
  throw_data <- merge(throw_data, game_data, by="at_bat", all.x=T)
  throw_data <- merge(throw_data, new_game_data, by.x="at_bat", by.y="next_at_bat")
  #getting the data for batter position at throw and catch
  batter_position_throw <- all_positions %>%
    arrange(timestamp) %>%
    filter(player_position==10) %>%
    reframe(batter_field_x_throw = field_x, batter_field_y_throw=field_y, timestamp, play_id)
  batter_position_catch <- all_positions %>%
    arrange(timestamp) %>%
    filter(player_position==10) %>%
    reframe(batter_field_x_catch = field_x, batter_field_y_catch=field_y, next_timestamp=timestamp, play_id)
  batter_position_catch <- all_positions %>%
    arrange(timestamp) %>%
    filter(player_position==10) %>%
    reframe(batter_field_x_catch = field_x, batter_field_y_catch=field_y, next_timestamp=timestamp, play_id)
  batter_position_next_frame <- all_positions %>%
    arrange(timestamp) %>%
    filter(player_position==10) %>%
    reframe(batter_field_x_next_frame = field_x, batter_field_y_next_frame=field_y, 
            timestamp=lag(timestamp, 15), play_id)
  #merging batter positions at time of throw and catch for sake of tracking
  throw_data <- merge(throw_data, batter_position_throw, by=c("timestamp", "play_id"), all.x=T)
  throw_data <- merge(throw_data, batter_position_catch, by=c("next_timestamp", "play_id"), all.x=T)
  throw_data <- merge(throw_data, batter_position_next_frame, by=c("timestamp", "play_id"), all.x=T)
  #tacking on game id
  throw_data$game_id <- file_index
  return(throw_data)}
#looping through function to get all 97 games in
processed_data <- reduce(lapply(1:97, clean_throw_data), rbind)
# calculating necessary variables for analysis
labeled_data<-processed_data %>%
  #calculating change in x and y coordinates
  mutate(x_change=next_frame_field_x - field_x,
         y_change=next_frame_field_y - field_y,
         z_change=next_frame_field_z-field_z,
         time_change=next_frame_timestamp1-timestamp) %>%
  #calculating hypotenuse and throw length at time of frame
  mutate(frame_hypotenuse=sqrt(x_change^2+y_change^2)) %>%
  #calculating z average change
  mutate(z_angle = atan(z_change/frame_hypotenuse)) %>%
  mutate(throw_velocity_2d=frame_hypotenuse/time_change, z_velocity=z_change/time_change) %>%
  #throw angle in radians based on y and x changes, with y as opposite and x as adjacent
  mutate(throw_angle_radians=atan2(y_change, x_change)) %>%
  #converting radians for degrees for sake of presentation
  mutate(throw_angle_degrees=throw_angle_radians*(180 / pi)) %>%
  #calculating throw slope
  mutate(throw_slope=tan(throw_angle_radians)) %>%
  #calculating hypothetical intersection of first base line based on throw angle
  mutate(first_base_arrival=ifelse(throw_angle_radians>0,
                                   ifelse(field_x<0,
                                          #all these equations were derived manually, and they differ based on the conditions
                                          #of the x coordinate and the throw angle
                                          sqrt(2*((tan(throw_angle_radians)*-field_x+field_y)/(1-tan(throw_angle_radians)))^2),
                                          sqrt(2*((tan(throw_angle_radians)*field_x-field_y)/(-1+tan(throw_angle_radians)))^2)),
                                   ifelse(field_x<0,
                                          sqrt(2*((tan(-throw_angle_radians)*-field_x-field_y)/(1+tan(-throw_angle_radians)))^2),
                                          sqrt(2*((tan(-throw_angle_radians)*field_x+field_y)/(1+tan(-throw_angle_radians)))^2)))) %>%
  #calculating throw
  mutate(ideal_throw_length = sqrt((sqrt(first_base_arrival^2/2)-field_x)^2+
                                     (sqrt(first_base_arrival^2/2)-field_y)^2) ) %>%
  #calculating the expected z at arrival
  mutate(z_at_arrival=z_velocity*ideal_throw_length/throw_velocity_2d-0.5*0.0321522*(ideal_throw_length/throw_velocity_2d)^2) %>%
  #calculating whether or not runner reached base at end of play. Does not account for inside the park home runs
  mutate(runner_reached_first = ifelse(batter==next_first_baserunner, 1, 0),
         runner_reached_second = ifelse(batter==next_second_baserunner, 1, 0),
         runner_reached_third = ifelse(batter==next_third_baserunner, 1, 0)) %>%
  #calculating the batter's x and y direction for sake of reference
  mutate(batter_x_change=batter_field_x_catch-batter_field_x_throw,
         batter_y_change=batter_field_y_catch-batter_field_y_throw) 
labeled_data$z_at_arrival
labeled_data$runner_reached_base <- rowSums(labeled_data[, c("runner_reached_first", 
                                         "runner_reached_second", 
                                        "runner_reached_third")], na.rm = TRUE)
#methodically filtering out inapplicable plays
within_range <- labeled_data %>%
  #first base arrival needs to be close to first, otherwise the throw was obviously not directed to first base
  filter(first_base_arrival <= 100, first_base_arrival >= 80,
         #the batter cannot have passed first base at the time of the throw
         batter_field_x_throw < 60, batter_field_y_throw < 60,
         #the throw has to have gone to the right
         x_change > 0,
         #some of the data is mislabeled
         batter != 0,
         #has to be throw by the shortstop, second baseman, or third baseman
         player_position %in% c(4, 5, 6),
         #the ball has to have been picked up around or past the first base area, no throw to first is getting cut off
         next_field_x>50,
         #the batter has to be moving to the right towards first
         batter_x_change>0,
         #the batter has to be moving up towards first
         batter_y_change>0)
#calculating the optimal place to catch the ball
ideal_catch_spot<-within_range %>%
  filter(runner_reached_base==0) %>%
  reframe(next_field_x=mean(next_field_x), next_field_y=mean(next_field_y), next_field_z=mean(next_field_z))
#calculating the optimal throw and the difference between optimal and observed
within_range<-within_range %>%
  mutate(optimal_x_change=ideal_catch_spot[1,1]-field_x,
         optimal_y_change=ideal_catch_spot[1,2]-field_y,
         optimal_z_change=ideal_catch_spot[1,3]-field_z) %>%
  mutate(optimal_throw_angle_radians=atan2((optimal_y_change), (optimal_x_change))) %>%
  mutate(optimal_throw_angle_degrees=optimal_throw_angle_radians*(180 / pi)) %>%
  mutate(raw_optimal_residual=((throw_angle_degrees-optimal_throw_angle_degrees))) %>%
  mutate(optimal_residual=abs((raw_optimal_residual + 180) %% 360 - 180))
#potentially useful model calculating probability of throw success based on some variables
field_position_model <- glm(runner_reached_base ~ field_x+field_y+
                              throw_angle_degrees+batter_field_x_throw+batter_field_y_throw, 
             data = within_range, family = binomial)
within_range$predicted_outs <- field_position_model$fitted.values
ggplot(data=within_range, aes(x=predicted_outs, y=runner_reached_base)) +
  geom_point() +
  geom_smooth(method="lm")
#optimal throw angle plot
optimal_resid_model <- glm(runner_reached_base ~ optimal_residual, 
    data = within_range, family = binomial)
summary(optimal_resid_model)
#important plot for difference
ggplot(data=within_range, aes(x=optimal_residual, y=runner_reached_base)) +
  geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial)) +
  xlab("Difference between ideal and observed throw angle calculated at time of catch") +
  ylab("Probability of batter reaching base") +
  ggtitle("How does an infielder's throw angle impact plays at first?")

ggplot(data=within_range, aes(x=z_at_arrival, y=runner_reached_base)) +
  geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial)) +
  xlab("Difference between ideal and observed throw angle calculated at time of catch") +
  ylab("Probability of batter reaching base") +
  ggtitle("How does an infielder's throw angle impact plays at first?")


#FUNCTION TO ANIMATE ANY GIVEN PLAY
#printing filename for sake of tracking
animate_play <- function(file_index, animated_play_id) {
print(paste("File index is ", file_index, sep=""))
#name of file based on game
game_id <- unlist(game_ids[file_index,])
#reading in various files for each game
ball_filename <- paste("ball_pos-", game_id, ".csv", sep="")
ball_pos <- read_csv(ball_filename)
ball_pos[1] <- NULL
game_events_filename <- paste("game_events-", game_id, ".csv", sep="")
game_events <- read_csv(game_events_filename)
game_events[1] <- NULL
game_info_filename <- paste("game_info-", game_id, ".csv", sep="")
game_info <- read_csv(game_info_filename)
game_info[1] <- NULL
player_pos_filename <- paste("player_pos-", game_id, ".csv", sep="")
player_pos <- read_csv(player_pos_filename)
player_pos[1] <- NULL
#binding player positioning and ball positioning into one
all_positions<-rbind(ball_pos %>%
                       reframe(
                         game_str, 
                         play_id,
                         timestamp,
                         field_x=ball_position_x, 
                         field_y=ball_position_y,
                         field_z=ball_position_z,
                         player_position="ball"),
                     player_pos %>%
                       reframe(
                         game_str, 
                         play_id,
                         timestamp,
                         field_x, 
                         field_y,
                         field_z=-999,
                         player_position))
#merging all of the data together so everything is IDed
all_positions<-merge(all_positions, game_events, by=c("game_str", "play_id", "timestamp", "player_position"), all.x=T)
all_positions <- merge(all_positions, game_info, by=c("game_str", "play_per_game", "at_bat"), all.x=T)
ggplot(data=all_positions %>%
         filter(play_id==animated_play_id, as.numeric(player_position)<14|player_position=="ball"), 
       aes(x=field_x, y=field_y)) +
  geom_point() +
  scale_x_continuous(limits=c(-200, 200)) +
  scale_y_continuous(limits=c(-10, 450)) +
  transition_states(timestamp, 
                    transition_length = 0.1, 
                    state_length = 0.1)
}
 