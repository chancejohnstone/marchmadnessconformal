dir <- getwd()

#ncaa_all <- read.csv("ncaaw_2014_2020_data.csv")
ncaa_all <- read.csv("current_ncaaw_data.csv")
#ncaa_all <- read.csv("current_ncaam_data.csv")

all_names <- read.csv("ncaa_clean_names_list.csv")
hold_away <- match(ncaa_all$AwayTeam, all_names$SeenTeams)
hold_home <- match(ncaa_all$HomeTeam, all_names$SeenTeams)

#not sure how to deal with new names other than give a warning
ncaa_all$AwayTeam <- all_names[hold_away,2]
ncaa_all$HomeTeam <- all_names[hold_home,2]

#check this to add names
new <- unique(ncaa_all$AwayTeam[is.na(hold_away)])
new <- unique(c(new, unique(ncaa_all$HomeTeam[is.na(hold_home)])))

ncaa_all$AwayTeam <- all_names[hold_away,2]
ncaa_all$HomeTeam <- all_names[hold_home,2]

i <- 1
game_data <- data.frame()
track <- 1
while(i <= nrow(ncaa_all)){
  #AwayTeam
  game_data[track,1] <- ncaa_all$HomeTeam[i] 
  #HomeTeam
  game_data[track,2] <-  ncaa_all$AwayTeam[i] 
  #home score minus away score
  game_data[track,3] <- ncaa_all$HomeScore[i] - ncaa_all$AwayScore[i]
  #determines whether this game is part of a conference tournament
  game_data[track,4] <- as.logical(ncaa_all$month[i] %in% c(3,4))
  #no info on neutral for ncaaw
  game_data[track,5] <- 0
  #year
  if(as.logical(ncaa_all$month[i] %in% c(1,2,3,4))){
    game_data[track,6] <- ncaa_all$year[i] - 1
  } else {
    game_data[track,6] <- ncaa_all$year[i]
  }
  
  i <- i + 1
  track <- track + 1
}

#backwards
game_data$V3 <- -game_data$V3

names(game_data) <- c("AwayTeam", "HomeTeam", "diff", "conf_tourn", "non_neutral", "year")
#ncaab <- game_data[game_data$conf_tourn == 0,]
ncaab <- game_data
#ncaab$conf_tourn <- NULL
#ncaab$non_neutral <- NULL
ncaab$day <- NULL

#write.csv(ncaab, "clean_ncaaw_current.csv")
write.csv(ncaab, "clean_ncaaw_current.csv")


