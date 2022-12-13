library(rvest)
library(dplyr)
library(readr)
library(stringr)

base <- "https://www.ncaa.com/scoreboard/basketball-women/d1/"

#needs to be in string format
#need to include all years when games are played,
#e.g., 2020 and 2021 all have games from the 2020-2021 season
year_vec_all <- as.character(seq(2014,2021))
#year_vec_all <- as.character(seq(2019,2020))

years <- matrix(0, nrow = length(year_vec_all)-1, ncol = 2)
for(i in 1:(length(year_vec_all)-1)){
  years[i,] <- c(year_vec_all[i], year_vec_all[i+1])
}
month_year <- list()
month_year[[1]] <- c("11","12")
month_year[[2]] <- c("01","02","03", "04")
month_vec <- c("11","12","01","02","03", "04")

#starting with january
month_day <- list()
month_day[[1]] <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#leap year
month_day[[2]] <- c(31,28,31,30,31,30,31,31,30,31,30,31)

day_vec <- c("01","02","03","04","05","06", "07","08","09","10",
             "11","12","13","14","15","16","17","18","19","20",
             "21","22","23","24","25","26","27","28","29","30","31")

all_data <- c()
all_hold <- list()
for(season in 1:nrow(years)){
  year_vec <- years[season,]
  for (y in 1:2){
    year <- year_vec[y]
    #identify leap years and use adjusted day vector
    if(year %in% c("2016", "2020")){
      month_day[[y]][2] <- 29  
    } else {
      month_day[[y]][2] <- 28  
    }
    for (month in month_year[[y]]){
      m <- parse_number(month)
      for(day in day_vec[1:month_day[[y]][m]]){
        
        date <- paste0(year,"/",month,"/",day,"/all-conf")
        paste0(base, date) -> url
        
        #catch 404 errors; happens with 2019-2020 season consistently
        error_catch <- tryCatch({
          
          game <- read_html(url) %>% 
            html_node("#scoreboardGames") %>%
            html_text()
          
          #getting each game
          final <- str_locate_all(game, "FINAL")[[1]]
          final <- rbind(final, str_locate_all(game, "Final")[[1]])
          
          #day_data <- c()
          str <- rep(0, times = nrow(final))
          score_data <- matrix(0,length(str), 4)
          if(dim(final)[1] != 0){
          #if(min(is.na(final)) != 1){
            for (t in 1:nrow(final)){
              
              skip <- FALSE
              
              #clean extra stuff from scrape
              str[t] <- substr(game, final[t,1]+40, final[t,2]+500)
              str[t] <- str_replace_all(str[t], "\n", "")
              str[t] <- str_replace_all(str[t], "Final", "")
              str[t] <- str_replace_all(str[t], "FINAL", "")
              str[t] <- str_replace_all(str[t], "Round", "")
              str[t] <- str_replace_all(str[t], "ROUND", "")
              str[t] <- str_replace_all(str[t], "First", "")
              str[t] <- str_replace_all(str[t], "Second", "")
              str[t] <- str_replace_all(str[t], "Third", "")
              str[t] <- str_replace_all(str[t], "Quarterfinals", "")
              str[t] <- str_replace_all(str[t], "Semifinals", "")
              str[t] <- str_replace_all(str[t], "Finals", "")
              str[t] <- str_replace_all(str[t], "Championship", "")
              str[t] <- str_replace_all(str[t], "FIRST", "")
              str[t] <- str_replace_all(str[t], "SECOND", "")
              str[t] <- str_replace_all(str[t], "THIRD", "")
              str[t] <- str_replace_all(str[t], "QUARTERFINALS", "")
              str[t] <- str_replace_all(str[t], "SEMIFINALS", "")
              str[t] <- str_replace_all(str[t], "FINALS", "")
              str[t] <- str_replace_all(str[t], "CHAMPIONSHIP", "")
              str[t] <- str_replace_all(str[t], "3OT", "")
              str[t] <- str_replace_all(str[t], "2OT", "")
              str[t] <- str_replace_all(str[t], "OT", "")
              str[t] <- str_replace_all(str[t], "\\(", "")
              str[t] <- str_replace_all(str[t], "\\)", "")
              str[t] <- str_replace_all(str[t], " ", "")
              str[t] <- str_replace_all(str[t], "-", "")
              str[t] <- str_replace_all(str[t], "\\.", "")
              str[t] <- trimws(str[t], which = "both")
              
              for(i in 1:2){
                if(str[t] != "") {
                  #check beginning of string for rank
                  keep_string <- str[t]
                  while (!is.na(parse_number(substr(str[t],1,1)))){
                    str[t] <- substr(str[t],2,nchar(str[t]))
                  }
                  
                  #step through to read name
                  track <- 1
                  count <- 1
                  while (is.na(parse_number(substr(str[t],1,track)))){
                    count <- count + 1
                    if(count > 250){
                      skip <- TRUE
                      break
                    }
                    track <- track + 1  
                  }
                  
                  if(!skip){
                  
                  #save team 
                  score_data[t,i] <- substr(str[t],1,track-1)
                  
                  #reduce string
                  str[t] <- substr(str[t],track,nchar(str[t]))
                  
                  
                  #logic for score parsing
                  str_hold <- as.character(parse_number(substr(str[t],1,5)))
                  if(nchar(str_hold) == 2){
                    #just a score less than 100 and no ranked second team
                    score_data[t,i+2] <- as.numeric(str_hold)
                  } else if(nchar(str_hold) == 3){
                    #could be high score or regular score with one digit rank
                    if(as.numeric(substr(str_hold,1,2)) >= 20){
                      #probably less high score with a one digit rank
                      score_data[t,i+2] <- as.numeric(substr(str_hold,1,2))
                    } else {
                      #probably high score with no rank after
                      score_data[t,i+2] <- as.numeric(str_hold)
                    }
                  } else if(nchar(str_hold) == 4){
                    #could be high score with a one digit rank, or regular score with two digit rank
                    #maybe dont need to spread logic
                    if(as.numeric(substr(str_hold,1,2)) >= 20){
                      #probably less high score with a one digit rank
                      score_data[t,i+2] <- as.numeric(substr(str_hold,1,2))
                    } else {
                      #probably high score with a single digit rank after
                      score_data[t,i+2] <- as.numeric(substr(str_hold,1,3))
                    }
                    
                  } else if(nchar(str_hold) == 5){
                    #high score with two digit rank
                    score_data[t,i+2] <- as.numeric(substr(str_hold,1,3))
                    
                  }
                  
                  str[t] <- substr(str[t],nchar(str_hold) + 1, nchar(str[t]))
                  }
                }
              }
              
              day_data <- data.frame(score_data, year, month, day)
            }
            
            #combine data from each day
            all_data <- rbind(all_data, day_data)
            
            #some issues with replicated data...
            all_data <- unique(all_data)
          }
        }, error = function(e) e)#end of error catch loop
      }
    }
  }
}

clean_data <- all_data[apply(all_data == 0, FUN = sum, MARGIN = 1) == 0,]
names(clean_data) <- c("HomeTeam", "AwayTeam", "HomeScore", "AwayScore","year", "month", "day")
#clean_data$diff <- clean_data$HomeScore - clean_data$AwayScore

#get clean names
all_names <- read.csv("ncaa_clean_names_list.csv")
hold_away <- match(clean_data$AwayTeam, all_names$SeenTeams)
hold_home <- match(clean_data$HomeTeam, all_names$SeenTeams)

#check this to add names
new <- unique(clean_data$AwayTeam[is.na(hold_away)])
new <- unique(c(new, unique(clean_data$HomeTeam[is.na(hold_home)])))

clean_data$AwayTeam <- all_names[hold_away,2]
clean_data$HomeTeam <- all_names[hold_home,2]

#uncomment to save scraped data
#write.csv(clean_data, "ncaaw_2020_2021_data_fix.csv")

