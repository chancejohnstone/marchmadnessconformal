dir <- getwd()
source(paste0(dir,"/sim_tournament.R"))
source(paste0(dir,"/gbg_res_gen.R"))

log_loss_func <- function(y, p){
  -sum((y*log(p) + (1-y)*log(1-p)))/length(y)
}

#import data from previous seasons; already cleaned
ncaa <- list()
ncaa[[1]] <- read.csv("ncaa basketball 2014-15.csv")
ncaa[[2]] <- read.csv("ncaa basketball 2015-16.csv")
ncaa[[3]] <- read.csv("ncaa basketball 2016-17.csv")
ncaa[[4]] <- read.csv("ncaa basketball 2017-18.csv")
ncaa[[5]] <- read.csv("ncaa basketball 2018-19.csv")
ncaa[[6]] <- read.csv("ncaa basketball 2019-20.csv")
ncaa[[7]] <- read.csv("ncaa basketball 2020-21.csv")

#label years
year_start <- 2014
for(i in 1:length(ncaa)){
  ncaa[[i]]$year <- 2014 - 1 + i
}

ncaa[[1]]$X <- NULL
ncaa_all <- rbind(ncaa[[1]], ncaa[[2]], ncaa[[3]], ncaa[[4]], ncaa[[5]], ncaa[[6]], ncaa[[7]])

#used to determine when conference tournament begin
season_start <- 1100
conf_tourn_start <- 228

#organize raw data into games
game_data <- data.frame()
track <- 1

i <- 1
while(i < nrow(ncaa_all)){
  #AwayTeam
  game_data[track,1] <- ncaa_all$Team[i] 
  #HomeTeam
  game_data[track,2] <-  ncaa_all$Team[i+1] 
  #home score minus away score
  game_data[track,3] <- ncaa_all$Final[i+1] - ncaa_all$Final[i]
  #date of game
  game_data[track, 4] <- ncaa_all$Date[i]
  #determines whether this game is part of a conference tournament
  game_data[track,5] <- as.logical((ncaa_all$Date[i] < season_start)*(ncaa_all$Date[i] >= conf_tourn_start))
  #homecourt advantage
  game_data[track,6] <- as.numeric(ncaa_all$VH[i+1] == "H")
  #year
  game_data[track,7] <- ncaa_all$year[i]
  
  i <- i + 2
  track <- track + 1
}

names(game_data) <- c("AwayTeam", "HomeTeam", "diff", "day", "conf_tourn", "non_neutral", "year")
#ncaab <- game_data[game_data$conf_tourn == 0,]
ncaab <- game_data
#ncaab$conf_tourn <- NULL
#ncaab$non_neutral <- NULL
ncaab$day <- NULL

#get clean names
all_names <- read.csv("ncaa_clean_names_list.csv")
hold_away <- match(ncaab$AwayTeam, all_names$SeenTeams)
hold_home <- match(ncaab$HomeTeam, all_names$SeenTeams)

#check this to add names
new <- unique(ncaab$AwayTeam[is.na(hold_away)])
new <- c(new, unique(ncaab$HomeTeam[is.na(hold_home)]))

ncaab$AwayTeam <- all_names[hold_away,2]
ncaab$HomeTeam <- all_names[hold_home,2]

write.csv(ncaab, "clean_ncaab_scores_2014_2020_all.csv")


