library("poibin")
library(dplyr)
library(readr)

#redo everything to fix rank issue with NorthernIowa

knockout_wp_conf <- function(slot, bracket, rbrack, round, index, train_data, ranks){
  
  ###
  slot <- 1
  bracket <- bracket_list
  rbrack <- rbrack
  round <- 1
  index <- S
  train_data <- train_data
  ranks <- ranks_all
  ###
  
  num_teams <- length(bracket)
  num_rounds <- log(length(rbrack),2)
  q <- matrix(0, nrow = num_teams, ncol = num_rounds)
  
  for(i in 1:num_rounds){
    for(j in 1:num_teams){
      if(i == 1){
        #only get wp for first game
        if(j %% 2 == 1){
          k <- j + 1
        } else {
          k <- j - 1
        }
        
        game <- data.frame(AwayTeam = ranks[bracket[k],1], 
                           HomeTeam = ranks[bracket[j],1], 
                           diff = 0, 
                           non_neutral = 0)
        
        #check for BYE
        if(sum("BYE" %in% c(game[,1:2])) != 0){
          q[j,1] <- as.numeric(game[,1] %in% "BYE")
        } else {
          aug <- train_data[1,]
          aug[1,] <- 0
          aug[1, colnames(aug) == game[,1]] <- -1
          aug[1, colnames(aug) == game[,2]] <- 1
          
          #need to fix this so we get wp for each team vs. the home team
          
          aug_data <- data.frame(rbind(train_data, aug))
          aug_data$int <- aug$non_neutral
          
          res <- lm(score~0+., data = aug_data)
          conf_scores <- -residuals(res)
          sign <- conf_scores >= 0
          cand <- conf_scores[nrow(aug_data)]
          conf_scores <- c(conf_scores, -conf_scores)
          
          pval <- sum(conf_scores < cand)/length(conf_scores) + 1/(2*length(conf_scores))
          
          q[j,1] <- pval
        }
        
      } else {
        #slot location start and end
        s <- index[j,i]
        t <- s + 2^(i-1) - 1
        for(k in s:t){
  
          game <- data.frame(AwayTeam = ranks[bracket[k],1], 
                             HomeTeam = ranks[bracket[j],1], 
                             diff = 0, 
                             non_neutral = 0)
          
          #check for BYE
          if(sum("BYE" %in% c(game[,1:2])) != 0){
            pval <- as.numeric(game[,1] %in% "BYE")
            q[j,i] <- q[j,i] + q[j,i-1]*q[k,i-1]*pval
          } else {
            aug <- train_data[1,]
            aug[1,] <- 0
            aug[1, colnames(aug) == game[,1]] <- -1
            aug[1, colnames(aug) == game[,2]] <- 1
            
            aug_data <- data.frame(rbind(train_data, aug))
            aug_data$int <- aug$non_neutral
            
            #implement updating scheme?
            res <- lm(score~0+., data = aug_data)
            conf_scores <- -residuals(res)
            sign <- conf_scores >= 0
            cand <- conf_scores[nrow(aug_data)]
            conf_scores <- c(conf_scores, -conf_scores)
            
            pval <- sum(conf_scores < cand)/length(conf_scores) + 1/(2*length(conf_scores))
            
            #conformal win probability
            q[j,i] <- q[j,i] + q[j,i-1]*q[k,i-1]*pval
          }
        }
      }
    }
    
    #check for double byes
    for(t in seq(1,nrow(q)-1, by = 2)){
      if(q[t,i] == 1 & q[t + 1,i] == 1){
        q[t+1,i] <- 0  
      }
    }
  }
  
  return(list(slot = slot, round = round, wp = q[slot,round], all_wp = q))
}

#get conference win probability for each team
methods <- expand.grid(c("m", "w"), c("conf"))
#methods <- methods[5:6,]
middle <- low <- list()
#1 is men; 2 is women
#middle[[1]] <- 36:42

#middle is situation 2 and 3
middle[[1]] <- c(40:47)
middle[[2]] <- 41:44
#low[[1]] <- 45:68
#low[[1]] <- low[[1]][!(low[[1]] %in% c(54,55,59))]

#low is situation 2 and 4
low[[1]] <- c(40:68)
low[[1]] <- low[[1]][!(low[[1]] %in% c(43,44,54,55,59))]
low[[2]] <- c(44,50,62)
total <- make_tourn <- list()
nteams <- list()
nteams[[1]] <- 68
nteams[[2]] <- 64
for(z in 1:nrow(methods)){
#for(z in 1){
  
  #get correct wp matrix
  if(methods[z,1] == "m"){
    #raw data
    #train_data <- read.csv("clean_ncaab_scores_2014_2020_all.csv")
    #train_data <- train_data[train_data$year == "2019",]
    #train_data <- train_data[train_data$conf_tourn == 0,]
    #train_data <- train_data %>% dplyr::select(diff, AwayTeam, HomeTeam, non_neutral)
    
    train_data <- readRDS("ncaa_train_data_m_2019.RDS")
    
    ncaa_conferences <- read.csv("ncaab_conference.csv")
    conf_complete_data <- read.csv("conf_complete_data_2020.csv")
    conf_tourn_schedule <- read.csv("conf_tournament_schedule.csv")
    ranks_all <- read.csv("ncaa_ranks_m_2019_sort.csv")
    team_index_low <- low[[1]]
    team_index_mid <- middle[[1]]
    nteams <- 68
    natlarge <- 36
    
  } else {
    #raw data
    #train_data <- read.csv("clean_ncaaw_scores_2014_2020_all.csv")
    #train_data <- train_data[train_data$year == "2019",]
    #train_data <- train_data[train_data$conf_tourn == 0,]
    #train_data <- train_data %>% dplyr::select(diff, AwayTeam, HomeTeam, non_neutral)
    
    train_data <- readRDS("ncaa_train_data_w_2019.RDS")
    
    ncaa_conferences <- read.csv("ncaaw_conference.csv")
    conf_complete_data <- read.csv("conf_complete_data_2020w.csv")
    conf_tourn_schedule <- read.csv("conf_tournament_schedulew.csv")
    ranks_all <- read.csv("ncaa_ranks_w_2019_sort.csv")
    team_index_low <- low[[2]]
    team_index_mid <- middle[[2]]
    nteams <- 64
    natlarge <- 32

  }
  
#execute conference tournaments
  #conference tournaments...
  conf_complete <- data.frame(unique(ncaa_conferences$Conference), NA)
  names(conf_complete) <- c("Conference", "winner")
  conf_complete <- conf_complete[conf_complete$Conference != "",]
  
  #filled complete conference champions
  match <- conf_complete$Conference %in% conf_complete_data$Conference
  conf_complete[match,2] <- as.character(conf_complete_data[,2])
  
  unfinished <- is.na(conf_complete$winner)
  all_conf <- conf_complete$Conference[unfinished]
  all_conf_done <- conf_complete$Conference[!unfinished]

  ranks <- as.numeric(ranks_all[,2])
  
  #closed form probability for winning conference tournament
  conf_wp <- rep(0, times = length(ranks))
  done <- match(na.omit(conf_complete$winner), ranks_all[,1])
  conf_wp[done] <- 1
  done_mat <- cbind(done <= nteams, done > nteams)
  done_conf <- which(!is.na(conf_complete[,2]))
  sum64 <- bottom <- matrix(0, nrow = nrow(conf_complete), ncol = 2)
  
  for(conf in all_conf){
    #get bracket for conference tournament
    games <- conf_tourn_schedule[conf_tourn_schedule$Conference == conf,2:3]
    
    #create new covariate vector
    #all <- rbind(train_data, games)
    
    #train <- train_data
    #test <- games
    
    #X <- model.matrix(~HomeTeam, all)
    #XV <- model.matrix(~AwayTeam, all)
    #new_all_X <- X - XV
    #future_games <- new_all_X[(nrow(train_data)+1):nrow(all)]
    
    bracket_list <- match(as.vector(t(as.matrix(conf_tourn_schedule[conf_tourn_schedule$Conference == conf,2:3]))),
                          ranks_all[,1])
    
    num_teams <- length(bracket_list)
    num_rounds <- log(num_teams,2)
    #if(num_rounds >= 2){
      S <- matrix(0, nrow = num_teams, ncol = num_rounds)
      for(i in 1:num_teams){
        for(j in 1:num_rounds){
          S[i,j] <- 1+2^(j+1)*floor((i-1)/(2^j)) + 2^(j-1) - 2^(j-1)*floor((i-1)/(2^(j-1)))
        }
      }
      
      sb <- sort(bracket_list, decreasing = FALSE)
      rbrack <- round(rank(bracket_list, ties.method = "random"))
      
      #wp_conf <- matrix(wp[as.matrix(expand.grid(sb,sb))], nrow = length(sb), ncol = length(sb))
      
      #need to put conformal win probs here; repeat for every potential game
      res <- knockout_wp_conf(1,bracket_list,rbrack,1,S,train_data,ranks_all)
      conf_wp[bracket_list] <- res$all_wp[,num_rounds]
      
    #} #else {
      #sb <- sort(bracket_list, decreasing = FALSE)
      #wp_conf <- t(matrix(wp[as.matrix(expand.grid(sb,sb))], nrow = length(sb), ncol = length(sb)))
      #conf_wp[bracket_list] <- wp_conf[c(2,3)]
    #}
    
    track <-match(conf, conf_complete[,1])
    top64 <- bracket_list <= nteams
    bottom[track,1] <- sum(conf_wp[bracket_list[top64]])
    bottom[track,2] <- sum(conf_wp[bracket_list[!top64]])
    
    sum64[track,1] <- sum(top64)
    sum64[track,2] <- sum(!top64)
    
    #print conference when finished
    print(conf)
    print(sum(res$all_wp[,ncol(res$all_wp)]))
    
  }
  
  for(conf in all_conf_done){
    pos <- match(conf,conf_complete$Conference)
    bracket <- match(ncaa_conferences$School[ncaa_conferences$Conference == conf],ranks_all[,1])
    sum64[pos,1] <- sum(bracket <= nteams)
    sum64[pos,2] <- sum(bracket > nteams)
  }
  
  
  
  bottom[done_conf,] <- as.numeric(done_mat)
  plot(conf_wp)
  
  file <- paste0("conf_wp_",methods[z,1],"_",methods[z,2],".csv")
  write.csv(conf_wp, file)

  keep <- as.logical(apply((bottom < 1)*(bottom > 0), FUN = sum, MARGIN = 1) == 2)
  
  all_conf_rem <- conf_complete$Conference[as.logical(unfinished*keep)]
  
  #mistake; add these
  if(methods[z,1] == "m"){
    all_conf_rem <- c(all_conf_rem, c("ACC", "Big Ten")) 
  }
  
  probs2 <- matrix(0,length(all_conf_rem), ncol = length(team_index_mid))
  for(t in team_index_mid){
    #lower <- ranks_all[(i+1):nrow(ranks_all),]
    bottom <- matrix(0, nrow = nrow(conf_complete), ncol = 2)
    
    for(conf in all_conf_rem){
      #get bracket for conference tournament
      games <- conf_tourn_schedule[conf_tourn_schedule$Conference == conf,2:3]
      bracket_list <- match(as.vector(t(as.matrix(conf_tourn_schedule[conf_tourn_schedule$Conference == conf,2:3]))),
                            ranks_all[,1])
      
      track <- match(conf, conf_complete[,1])
      top <- bracket_list <= t
      bottom[track,1] <- sum(conf_wp[bracket_list[top]])
      bottom[track,2] <- sum(conf_wp[bracket_list[!top]])
      
    }
    spot <- sort(match(all_conf_rem, conf_complete$Conference))
    probs2[,t-(min(team_index_mid - 1))] <- bottom[spot,2]
  }
  
  probs2[probs2 >= .999999] <- .999999
  probs2[probs2 <- .0000001] <- .0000001
  probs2[probs2 >= .999999] <- 1
  probs2[probs2 <- .0000001] <- 0
  #make tournament as at-large bid
  make <- list()
  for(i in 1:length(team_index_mid)){
    make[[i]] <- ppoibin(seq(0,32),probs2[,i], wts = rep(1, times = length(probs2[,i])))[length(team_index_mid)-i+1]
  }
  
  #intersection
  if(methods[z,1] == "m"){
    #check_cond <- rep(0, times = 8)
    ord <- length(team_index_mid):1
    inter <- rep(0, times = length(ord))
    for(k in ord[ord]){
      id <- match(ranks_all[team_index_mid[k],1], ncaa_conferences[,1])
      conf <- ncaa_conferences[id,2]
      spot <- sort(match(all_conf_rem, conf_complete$Conference))
      conf_match <- match(conf, conf_complete$Conference[spot])
      if(is.na(conf_match)){
        inter[k] <- ppoibin(ord[k]-1,probs2[,k])
        #inter[k] <- ppoibin(ord[k]+1,probs2[,k])
      } else {
        inter[k] <- ppoibin(ord[k]-1,probs2[-conf_match,k]) 
        #inter[k] <- ppoibin(ord[k]+1,probs2[-conf_match,k])
      }
      #print(ord[z] - 1)
      #print(inter[z])
    }
    
    conf_wp2 <- conf_wp[team_index_mid]
    joint <- inter*conf_wp2
  } else {
    conf_wp2 <- conf_wp[team_index_mid]
    joint <- rep(0, times = length(team_index_mid))
  }

total[[z]] <- unlist(make) + conf_wp2 - joint
make_tourn[[z]] <- rep(0, times = length(conf_wp))
make_tourn[[z]][1:(min(team_index_mid)-1)] <- 1
make_tourn[[z]][team_index_mid] <- total[[z]]
make_tourn[[z]][team_index_low] <- conf_wp[team_index_low]
make_tourn[[z]][(nteams+1):length(conf_wp)] <- conf_wp[(nteams+1):length(conf_wp)]
make_tourn[[z]][make_tourn[[z]] <= .00001] <- 0
}#end big loop

#m_make_tourn_mat <- cbind(total[[1]], total[[3]], total[[5]], total[[7]])
#w_make_tourn_mat <- cbind(total[[2]], total[[4]], total[[6]], total[[8]])
m_make_tourn_mat <- cbind(total[[1]])
w_make_tourn_mat <- cbind(total[[2]])

matplot(m_make_tourn_mat, type = "l")
legend("topright", legend = c(as.character(methods[c(2,4,6,7),2])), lty = 1:4, col = 1:4)
matplot(w_make_tourn_mat, type = "l")
legend("topright", legend = c(as.character(methods[c(2,4,6,7),2])), lty = 1:4, col = 1:4)

#finish rank probs
#set up bracket
#get tournament win probs

