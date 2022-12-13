library(dplyr)

knockout_wp <- function(slot, bracket, round, index, wp_mat){
  #num_teams <- nrow(wp_mat)
  num_teams <- length(bracket)
  num_rounds <- log(length(bracket),2)
  q <- matrix(0, nrow = num_teams, ncol = num_rounds)
  
  #first round win probabilities
  f <- cbind(bracket, bracket[index[,1]])
  q[,1] <- wp_mat[f]
  
  #rearrange wp_mat
  wp_mat <- wp_mat[,bracket]
  wp_mat <- wp_mat[bracket,]
  for(i in 2:num_rounds){
    for(j in 1:num_teams){
      #slot location start and end
      s <- index[j,i]
      t <- s + 2^(i-1) - 1
      for(k in s:t){
        q[j,i] <- q[j,i] + q[j,i-1]*q[k,i-1]*wp_mat[j,k]
      }
    }
  }
  
  return(list(slot = slot, round = round, wp = q[slot,round], all_wp = q))
}

source("knockout_wp_conf.R")

set.seed(2020)
all_file_id <- c()

all_bids <- list()
first_four <- list()
num <- 1
for(l in c("m", "w")){
  if(l == "m"){
    ncaa_conferences <- read.csv("ncaab_conference.csv")
    conf_complete_data <- read.csv("conf_complete_data_2020.csv")
    conf_tourn_schedule <- read.csv("conf_tournament_schedule.csv")
    ranks_all <- read.csv("ncaa_ranks_m_2019_sort.csv")
    conf_wp <- read.csv("conf_wp_m_conf.csv")[,2]
    num_teams <- 68
    curve <- read.csv("s_curve.csv", header = FALSE)
  } else {
    ncaa_conferences <- read.csv("ncaaw_conference.csv")
    conf_complete_data <- read.csv("conf_complete_data_2020w.csv")
    conf_tourn_schedule <- read.csv("conf_tournament_schedulew.csv")
    ranks_all <- read.csv("ncaa_ranks_w_2019_sort.csv")
    conf_wp <- read.csv("conf_wp_w_conf.csv")[,2]
    num_teams <- 64
    curve <- read.csv("s_curve.csv", header = FALSE)
  }

conf_complete <- data.frame(unique(ncaa_conferences$Conference), NA)
names(conf_complete) <- c("Conference", "winner")
conf_complete <- conf_complete[conf_complete$Conference != "",]

#filled complete conference champions
match <- conf_complete$Conference %in% conf_complete_data$Conference
conf_complete[match,2] <- as.character(conf_complete_data[,2])

ranks <- as.numeric(ranks_all[,2])

rank_mat <- matrix(0, nrow = nrow(ranks_all), ncol = num_teams)
B <- 100000
save_brack <- matrix(0, num_teams, B)

btrack <- 1

#max bracket
track <- 1
pick <- rep(0, times = 32)
for(conf in conf_complete$Conference){
  teams <- match(ncaa_conferences$School[ncaa_conferences$Conference %in% conf], ranks_all[,1])
  teams <- teams[!is.na(teams)]
  probs <- conf_wp[teams]
  
  if(sum(probs > .9999) == 1){
    pick[track] <- teams[as.logical(probs)]
  } else {
    out <- as.logical(apply(cbind(probs < .00001, probs > .9999), FUN = max, MARGIN = 1))
    probs <- probs[!out]
    teams <- teams[!out]
    cum_probs <- c(0,cumsum(probs))
    
    #pick highest ranked team
    pick[track] <- teams[which.min(teams)]
  }
  
  track <- track + 1
}

fill <- (1:num_teams)[!(1:num_teams %in% pick)][1:(num_teams - 32)]

bids <- cbind(sort(c(pick, fill)), 1:num_teams)

max_bid <- bids

if(l == "m"){
  last <- length(fill):33
  first_four[[btrack]] <- c(fill[last], sort(pick)[32:29])
}
btrack <- btrack + 1

#min bracket
track <- 1
pick <- rep(0, times = 32)
for(conf in conf_complete$Conference){
  teams <- match(ncaa_conferences$School[ncaa_conferences$Conference %in% conf], ranks_all[,1])
  teams <- teams[!is.na(teams)]
  probs <- conf_wp[teams]
  
  if(sum(probs > .9999) == 1){
    pick[track] <- teams[as.logical(probs)]
  } else {
    out <- as.logical(apply(cbind(probs < .00001, probs > .9999), FUN = max, MARGIN = 1))
    probs <- probs[!out]
    teams <- teams[!out]
    cum_probs <- c(0,cumsum(probs))
    #r <- runif(1)
    
    #pick lowest ranked team
    pick[track] <- teams[which.max(teams)]
  }
  
  track <- track + 1
}

fill <- (1:num_teams)[!(1:num_teams %in% pick)][1:(num_teams - 32)]

bids <- cbind(sort(c(pick, fill)), 1:num_teams)

min_bid <- bids

if(l == "m"){
  last <- length(fill):33
  first_four[[btrack]] <- c(fill[last], sort(pick)[32:29])
}
btrack <- btrack + 1

#random bracket
track <- 1
pick <- rep(0, times = 32)
for(conf in conf_complete$Conference){
  teams <- match(ncaa_conferences$School[ncaa_conferences$Conference %in% conf], ranks_all[,1])
  teams <- teams[!is.na(teams)]
  probs <- conf_wp[teams]
  
  if(sum(probs > .9999) == 1){
    pick[track] <- teams[as.logical(probs)]
  } else {
    out <- as.logical(apply(cbind(probs < .00001, probs > .9999), FUN = max, MARGIN = 1))
    probs <- probs[!out]
    teams <- teams[!out]
    cum_probs <- c(0,cumsum(probs))
    r <- runif(1)
    pick[track] <- teams[as.logical((r <= (dplyr::lead(cum_probs))[-length(cum_probs)])*(r > cum_probs[-length(cum_probs)]))]
  }
  
  track <- track + 1
}

#fill with at large bids
fill <- (1:num_teams)[!(1:num_teams %in% pick)][1:(num_teams - 32)]

bids <- cbind(sort(c(pick, fill)), 1:num_teams)

random_bid <- bids

all_bids[[num]] <- cbind(max_bid[,1], min_bid[,1], random_bid[,1])

if(l == "m"){
  last <- length(fill):33
  first_four[[btrack]] <- c(fill[last], sort(pick)[32:29])
}
btrack <- btrack + 1

bracket_vec <- c("max", "min", "random")

#first four stuff...
first_four_vec <- t(matrix(c(4,1,3,2,8,5,7,6), nrow = 2))
if(l == "m"){
  for(b in 1:3){
    for(p in 1:4){
      id <- match(first_four[[b]][first_four_vec[p,1]], all_bids[[1]][,b])
      id2 <- match(first_four[[b]][first_four_vec[p,2]], all_bids[[1]][,b])
      all_bids[[1]][id,b] <- paste0(all_bids[[1]][id,b],"/",all_bids[[1]][id2,b])
      all_bids[[1]][id2,b] <- NA
    } 
  }

  #resize matrix
  all_bids[[1]] <- cbind(all_bids[[1]][!is.na(all_bids[[1]][,1]),1],
                         all_bids[[1]][!is.na(all_bids[[1]][,2]),2],
                         all_bids[[1]][!is.na(all_bids[[1]][,3]),3])  
}

for(b in 1:3){
  first_four_edit <- first_four
  #make bracket
  bracket <- bracket_id <-  c()
  #games <- c(1,8,6,4,3,5,7,2)
  games <- c(1,8,5,4,6,3,7,2)
  for(i in c(1,4,2,3)){
    reg_bracket <- reg_bracket_id <- c()
    reg_teams <- all_bids[[num]][curve[,i],b]
    for(j in 1:8){
      
      teams_id <- c(j,16 - j + 1)
      
      if(grepl("/", reg_teams[teams_id][2])){
        #parse out the two teams
        loc <- regexpr("/", reg_teams[teams_id][2]) - 1
        teamA <- ranks_all[as.numeric(substr(reg_teams[teams_id][2], 1 ,loc)),1]
        teamB <- ranks_all[as.numeric(substr(reg_teams[teams_id][2], loc + 2, nchar(reg_teams[teams_id][2]))),1]
        couple <- paste0(teamA,"/",teamB)
        reg_bracket <- rbind(reg_bracket, c(ranks_all[reg_teams[teams_id[1]],1],couple))
        reg_bracket_id <- rbind(reg_bracket_id, c(reg_teams[teams_id])) 
        ####      
      } else {
        reg_bracket <- rbind(reg_bracket, c(ranks_all[reg_teams[teams_id],1]))
        reg_bracket_id <- rbind(reg_bracket_id, c(reg_teams[teams_id]))  
      }

    }
    
    #reorder bracket to match actual bracket structure
    reg_bracket <- reg_bracket[games,]
    reg_bracket_id <- reg_bracket_id[games,]
    
    #add to full bracket
    bracket <- rbind(bracket, reg_bracket)
    bracket_id <- rbind(bracket_id, reg_bracket_id)
  }
  
  file <- paste0("final_bracket_",l,"_", bracket_vec[b],".csv")
  file_id <- paste0("final_bracket_",l,"_", bracket_vec[b],"_id.csv")
  file_list <- paste0("final_bracket_",l,"_", bracket_vec[b],"_list.csv")
  write.csv(c(t(bracket)), file)
  write.csv(c(t(bracket_id)), file_id)
  write.csv(ranks_all[all_bids[[num]][,b],1], file_list)
  
  all_file_id <- c(all_file_id, file_id)
}

num <- num + 1
}

#write.csv(all_file_id, "file_id_list.csv")

#execute brackets and get win probs
all_file_id <- read.csv("file_id_list.csv")[,2]
expert_brackets <- c("lunardi_bracket_id.csv",
                     "palm_bracket_id.csv",
                     "katz_bracket_id.csv",
                     "smith_bracket_id.csv",
                     "csm_bracket_id.csv",
                     "rtrpi_bracket_id.csv")
expert_brackets <- paste0("expert_", expert_brackets)
all_file_id <- c(all_file_id[1:3],
                 expert_brackets[1:3],
                 all_file_id[4:6],
                 expert_brackets[4:6])

mwp_list <- c("oos_wp_2019.RDS",
             "oos_wp_2019.RDS",
             "norm_wp_m_2019.RDS",
             "BT_wp_m_2020.RDS")
wwp_list <- c("oos_wp.RDS",
              "oos_wp.RDS",
              "norm_wp_w_2019.RDS",
              "BT_wp_w_2019.RDS")
name_vec <- rep(c("max", "min", "random", "expert1", "expert2", "expert3"), times = 2)
mlist_vec <- c(1,2,1,1)
wlist_vec <- c(3,4,2,2)
#palm bracket doesn't have first four, so we only choose 64 teams
num_teams_list <- c(68,68,68,68,64,68,64,64,64,64,64,64)
method_vec <- c("seq", "dfold", "norm", "BT")
all_wp <- all_wp_rounds <- list()
#for(q in 1:12){
for(q in 10:12){
#for(q in 12){
  if(q <= 6){
    bye <- 354
    train_data <- readRDS("ncaa_train_data_m_2019.RDS")
    ranks_all <- read.csv("ncaa_ranks_m_2019_sort.csv")
  } else {
    bye <- 351
    train_data <- readRDS("ncaa_train_data_w_2019.RDS")
    ranks_all <- read.csv("ncaa_ranks_w_2019_sort.csv")
  }
  
  expert <- grep("expert", all_file_id[q])
  if(length(expert) == 1){
    #first four not added for lunardi and katz
    expert_file <- sub("expert_", "", all_file_id[q])
    bracket_id <- read.csv(expert_file)[,2]
  } else {
    bracket_id <- read.csv(all_file_id[q])[,2]
  }
  bracket <- cbind(bracket_id, bye)
  loc <- regexpr("/", bracket[,1]) - 1
  keep <- which(loc > 0)
  teamA <- as.numeric(substr(bracket[,1], 1 ,loc))
  teamB <- as.numeric(substr(bracket[,1], loc + 2, 10))
  bracket[keep,1] <- teamA[!is.na(teamA)]
  bracket[keep,2] <- teamB[!is.na(teamA)]
  
  bracket <- as.numeric(c(t(bracket)))
  rbrack <- round(rank(bracket, ties.method = "random"))
  
  #includes all bye teams
  num_teams <- 128
  
  #includes all by rounds
  num_rounds <- 7
  
  S <- matrix(0, nrow = num_teams, ncol = num_rounds)
  for(i in 1:num_teams){
    for(j in 1:num_rounds){
      S[i,j] <- 1+2^(j+1)*floor((i-1)/(2^j)) + 2^(j-1) - 2^(j-1)*floor((i-1)/(2^(j-1)))
    }
  }
  
  final <- matrix(0, nrow = num_teams_list[q], ncol = 5)
  #for(k in 1:4){
  #  if(q <= 3){
      #cleaned data?
  #    wp <- readRDS(mwp_list[k])[[mlist_vec[k]]]  
  #  } else {
  #    wp <- readRDS(wwp_list[k])[[wlist_vec[k]]] 
  #  }
    
    #contour(wp, levels = seq(.1,.9,.1))
    
  #  round <- 2
  #  slot <- 1
  #  index <- S
  #  wp_mat <- wp
    
  #  res <- knockout_wp(1,bracket,2,S,wp)
    
  #  final[,k] <- res$all_wp[bracket < bye, num_rounds]
  #}
  
  #conformal
  res <- knockout_wp_conf(1,bracket,rbrack,1,S,train_data,ranks_all)
  k <- 5
  
  final[,k] <- res$all_wp[bracket < bye, num_rounds]
  
  all_wp_rounds[[q]] <- res$all_wp[bracket < bye,]
  
  final <- final[order(bracket[bracket < bye]),]
  
  all_wp_rounds[[q]] <- all_wp_rounds[[q]][order(bracket[bracket < bye]),]
  
  all_wp[[q]] <- final
  
  matplot(final[1:10,], type = "l", ylim = c(0,.35), main = name_vec[q])
  
}

all_wp_mat <- c()

all_wp_mat <- rbind(all_wp[[1]][1:10,],
                    all_wp[[2]][1:10,],
                    all_wp[[3]][1:10,],
                    all_wp[[4]][1:10,],
                    all_wp[[5]][1:10,],
                    all_wp[[6]][1:10,],
                    all_wp[[7]][1:10,],
                    all_wp[[8]][1:10,],
                    all_wp[[9]][1:10,],
                    all_wp[[10]][1:10,],
                    all_wp[[11]][1:10,],
                    all_wp[[12]][1:10,])

all_wp_mat_all <- rbind(all_wp[[1]],
                    all_wp[[2]],
                    all_wp[[3]],
                    all_wp[[4]],
                    all_wp[[5]],
                    all_wp[[6]],
                    all_wp[[7]],
                    all_wp[[8]],
                    all_wp[[9]],
                    all_wp[[10]],
                    all_wp[[11]],
                    all_wp[[12]])
write.csv(all_wp_mat, "all_wp_conf.csv")
write.csv(all_wp_mat_all, "all_wp_conf_good.csv")

wp_csv <- read.csv("all_wp_conf_good.csv")[,6]
cum_sum <- cumsum(num_teams_list)
index <- 1:max(cum_sum)
cuts <- cut(index, breaks = c(0,cum_sum,max(cum_sum) + 1), labels = FALSE)

all_wp <- list()
for(c in 1:max(cuts)){
  all_wp[[c]] <- wp_csv[cuts == c]
}
mavg_wp <- read.csv("ncaa_m_bracket_avg_wp.csv")
wavg_wp <- read.csv("ncaa_w_bracket_avg_wp.csv")

top25m <- mavg_wp[1:25,]
top25w <- wavg_wp[1:25,] 

w <- 3
h <- 3
par(pin = c(h,w), mfrow = c(1,2))
#plot(mavg_wp$avg_men[1:25], pch = NA, ylim = c(0,.3), 
#     ylab = "win probability range", xlab = "average win probability rank",
#     cex.lab = 1.2)
holdm <- holdw <- c()
for(q in 1:6){
  bye <- 354
  expert <- grep("expert", all_file_id[q])
  if(length(expert) == 1){
    #first four not added for lunardi and katz
    expert_file <- sub("expert_", "", all_file_id[q])
    bracket_id <- read.csv(expert_file)[,2]
  } else {
    bracket_id <- read.csv(all_file_id[q])[,2]
  }
  bracket <- cbind(bracket_id, bye)
  loc <- regexpr("/", bracket[,1]) - 1
  keep <- which(loc > 0)
  teamA <- as.numeric(substr(bracket[,1], 1 ,loc))
  teamB <- as.numeric(substr(bracket[,1], loc + 2, 10))
  bracket[keep,1] <- teamA[!is.na(teamA)]
  bracket[keep,2] <- teamB[!is.na(teamA)]
  
  bracket <- as.numeric(c(t(bracket)))
  bracket <- sort(unique(bracket))
  
  vals <- all_wp[[q]][match(top25m$rank, bracket)]
  #points(x = top25m$rank, y = vals, col = q, pch = 20)
  
  holdm <- cbind(holdm, vals)
}
ord <- order(top25m$rank)
#lines(x = mavg_wp$rank[ord], y = mavg_wp$avg_men[ord], lwd = 2)
plot(mavg_wp$avg_men[1:25], pch = NA, ylim = c(0,.3), 
     ylab = "win probability range", xlab = "average win probability rank",
     cex.lab = 1.2)

holdm[is.na(holdm)] <- 0
holdm <- data.frame(holdm)
holdm_summary <- t(apply(holdm, FUN = range, MARGIN = 1))
#segments(y0 = holdm_summary[,1], x0 = top25m$rank, y1 = holdm_summary[,2], lwd = 4)
segments(y0 = holdm_summary[,1], x0 = 1:25, y1 = holdm_summary[,2], lwd = 4)
#boxplot(t(holdm))

#plot(wavg_wp$avg_women[1:25], pch = NA, ylim = c(0,1), 
#     ylab = "win probability range", xlab = "average win probability rank",
#     cex.lab = 1.2)

for(q in 7:12){
  bye <- 351
  expert <- grep("expert", all_file_id[q])
  if(length(expert) == 1){
    #first four not added for lunardi and katz
    expert_file <- sub("expert_", "", all_file_id[q])
    bracket_id <- read.csv(expert_file)[,2]
  } else {
    bracket_id <- read.csv(all_file_id[q])[,2]
  }
  bracket <- cbind(bracket_id, bye)
  loc <- regexpr("/", bracket[,1]) - 1
  keep <- which(loc > 0)
  teamA <- as.numeric(substr(bracket[,1], 1 ,loc))
  teamB <- as.numeric(substr(bracket[,1], loc + 2, 10))
  bracket[keep,1] <- teamA[!is.na(teamA)]
  bracket[keep,2] <- teamB[!is.na(teamA)]
  
  bracket <- as.numeric(c(t(bracket)))
  bracket <- sort(unique(bracket))
  
  vals <- all_wp[[q]][match(top25w$rank, bracket)]
  #cum_sum <- cumsum(all_wp[[q]][match(top25w$rank, bracket)])
  #lines(x = top25w$rank, y = cum_sum, col = q)
  
  holdw <- cbind(holdw, vals)
}
ord <- order(top25w$rank)
#lines(x = wavg_wp$rank[ord], y = wavg_wp$avg_women[ord], lwd = 2)

plot(wavg_wp$avg_women[1:25], pch = NA, ylim = c(0,.3), 
     ylab = "win probability range", xlab = "average win probability rank",
     cex.lab = 1.2)
holdw[is.na(holdw)] <- 0
holdw_summary <- t(apply(holdw, FUN = range, MARGIN = 1))
#segments(y0 = holdw_summary[,1], x0 = top25w$rank, y1 = holdw_summary[,2], lwd = 4)
segments(y0 = holdw_summary[,1], x0 = 1:25, y1 = holdw_summary[,2], lwd = 4)

w <- 4
h <- 4
par(pin = c(h,w))
ord <- 1:25
plot(x = ord, y = holdw[ord,4], type = "l", pch = 20, ylim = c(0,.3), 
     ylab = "win probability", xlab = "average win probability rank",
     cex.lab = 1.2, lwd = 2)
points(x = ord, y = holdw[ord,4], pch = 20)
lines(x = ord, y = holdw[ord,5], col = "red", lwd = 2, lty = 2)
points(x = ord, y = holdw[ord,5], pch = 17, col = "red")
lines(x = ord, y = holdw[ord,6], col = "blue", lwd = 2, lty = 3)
points(x = ord, y = holdw[ord,6], pch = 3, col = "blue",)  
legend("topright", legend = c("Smith", "CSM", "RTRPI"), col = c(1,"red","blue"), 
       pch = c(20,17,3), lty = c(1,2,3), lwd = 2)

#ord <- order(top25m$rank)
plot(x = ord, y = holdm[ord,4], type = "l", pch = 20, ylim = c(0,.3), 
     ylab = "win probability", xlab = "average win probability rank",
     cex.lab = 1.2, lwd = 2)
points(x = ord, y = holdm[ord,4], pch = 20)
lines(x = ord, y = holdm[ord,5], col = "red", lwd = 2, lty = 2)
points(x = ord, y = holdm[ord,5], pch = 17, col = "red")
lines(x = ord, y = holdm[ord,6], col = "blue", lwd = 2, lty = 3)
points(x = ord, y = holdm[ord,6], pch = 3, col = "blue",)  
legend("topright", legend = c("Lunardi", "Palm", "Katz"), col = c(1,"red","blue"), 
       pch = c(20,17,3), lty = c(1,2,3), lwd = 2)

#write.csv(all_wp[[10]], "new_smith_wp.csv")
w <- 4
h <- 6
par(pin = c(h,w))
plot(all_wp_rounds[[10]][1,-1], ylim = c(0,1), ylab = "round win probability", xlab = "round", 
     pch = NA, cex.lab = 1.5,
     xaxt = "n")
for(q in 10:12){
  points(all_wp_rounds[[q]][1,-1], ylim = c(0,1), ylab = "round win probability", xlab = "round", 
       pch = q-9, col = 1, cex = 1.5, lwd = 2)  
  lines(all_wp_rounds[[q]][1,-1], ylim = c(0,1), ylab = "round win probability", xlab = "round", 
        col = 1, lty = q-9, lwd = 2)
  
  points(all_wp_rounds[[q]][2,-1], ylim = c(0,1), 
         pch = q-9, col = 2, cex = 1.5,  lwd = 2)  
  lines(all_wp_rounds[[q]][2,-1], ylim = c(0,1),
        col = 2, lty = q-9, lwd = 2)
}

axis(1, at = c(1:6), 
     labels = c("1st Round", "2nd Round", "Sweet Sixteen", "Elite Eight", "Final Four", "Finals"))

abline(h = seq(0,1,by = .2), lty = 2)
legend("bottomleft", legend = c("Baylor-Smith", 
                                "Baylor-CSM", 
                                "Baylor-RTRPI",
                                "South Carolina-Smith",
                                "South Carolina-CSM",
                                "South Carolina-RTRPI"),
       pch = c(1,2,3,1,2,3), col = c(1,1,1,2,2,2), lty = c(1,2,3,1,2,3), lwd = 2)
