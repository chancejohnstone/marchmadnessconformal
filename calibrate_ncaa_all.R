#fix issues with by_fave and observed win/loss

library(dplyr)
library(ggplot2)
library(Iso)
library(doParallel)
library(parallel)

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

rotate <- function(x) t(apply(x, 2, rev))

year_vec <- 2014:2020
methods <- c("norm", "BT", "conf")

#checking for issues
#methods <- c("norm", "BT")

loss_year <- rep( list(list()), length(year_vec))
loss <- data.frame()
all_lECE <- all_lwECE <- all_probs <- c()
full_probs <- rep( list(c()), length(methods))
leagues <- c("m", "w")

all_pval <- c()

#turn win probs into fave probs
by_fave <- FALSE

#year_vec <- 2019
for(i in 1:length(year_vec)){
  for(l in leagues){
  file_name_rank <- paste0("ncaa_ranks_", l, "_", year_vec[i], "_sort.csv")
  ranks_all <- read.csv(file_name_rank)
  ranks_all[,1] <- gsub("`","",ranks_all[,1])
  file_name_train <- paste0("ncaa_train_data_", l, "_", year_vec[i], ".RDS")
  file_name_test <- paste0("ncaa_test_data_", l, "_", year_vec[i], ".RDS")
  file_name_test_mat <- paste0("ncaa_test_matrix_data_", l, "_", year_vec[i], ".RDS")
  train_data <- readRDS(file_name_train)
  test_data <- readRDS(file_name_test)
  
  #model
  model <- readRDS("final_model_m_2014.RDS")
  
  #need to create test data matrix
  test_data_mat <- readRDS(file_name_test_mat)
  for(j in 1:length(methods)){
    if(methods[j] == "seq"){
      file_name_wp <- paste0("oos_wp_", l, "_", year_vec[i], ".RDS") 
      wp <- readRDS(file_name_wp)[[1]]
    } else if(methods[j] == "dfold"){
      file_name_wp <- paste0("oos_wp_", l, "_", year_vec[i], ".RDS")  
      wp <- readRDS(file_name_wp)[[2]]
    } else if(methods[j] == "BT"){
      file_name_wp <- paste0(methods[j], "_wp_", l, "_", year_vec[i], ".RDS") 
      wp <- readRDS(file_name_wp)[[1]]
    } else if(methods[j] == "norm"){
      file_name_wp <- paste0(methods[j], "_wp_", l, "_", year_vec[i], ".RDS") 
      wp <- readRDS(file_name_wp)[[1]]
    } #else {
      #file_name_wp <- paste0("oos_wp_", l, "_", year_vec[i], ".RDS")  
      #start_wp <- readRDS(file_name_wp)[[1]]
      #wp <- rotate(rotate(rotate(biviso(rotate(start_wp)))))
    #}
   
    #home <- match(test_data[,2], ranks_all[,1])
    #away <- match(test_data[,1], ranks_all[,1])
    home <- match(test_data$HomeTeam, ranks_all[,1])
    away <- match(test_data$AwayTeam, ranks_all[,1])
    all_comb_sort <- cbind(away, home)
    
    #estimated and empirical for home team win
    probs <- 1 - wp[all_comb_sort]
    exp <- test_data$diff > 0
    
    if(by_fave){
    #get in terms of favorite
    check <- probs <= .5
    change_exp <- exp
    change_exp[check] <- 1 - as.numeric(as.logical(change_exp[check]))
      
    #get in terms of favorite
    #both_probs <- cbind(probs, 1-probs)
    #fave <- apply(both_probs, FUN = which.max, MARGIN = 1)
    #fave_exp <- (-fave + 3) == (exp + 1)
    
    #probs <- apply(both_probs, FUN = max, MARGIN = 1)
    #exp <- fave_exp
    
    probs[check] <- 1 - probs[check]
    exp <- change_exp
    }
    
    
    #fix this
    #parallel
    ###in parallel; depends on which machine is running it
    num_threads <- detectCores() - 1
    if(is.null(num_threads)){
      num_threads <- detectCores() - 1
    } else {
      num_threads = num_threads
    }
    
    if(getDoParWorkers() != num_threads){
      clust <- makeCluster(num_threads)
      registerDoParallel(clust)
    }
    ####

    set.seed(2020)
    
    #can run in parallel with above chunk
    if(methods[j] == "conf"){
      prob_data <- foreach(m = 1:nrow(test_data), .combine = 'comb') %dopar% {
      #prob_data <- foreach(m = 1:nrow(test_data), .combine='comb', .multicombine=TRUE,
      #                 .init=list(list(), list())) %dopar% {
        aug_data <- rbind(train_data, test_data_mat[m,])
        aug_data$score[nrow(aug_data)] <- 0
        
        res <- lm(score~., data = aug_data)
        #res$residuals <- c(res$residuals, -res$residuals)
        p <- 1 - (sum(res$residuals < res$residuals[nrow(aug_data)]) + .5*sum(res$residuals == res$residuals[nrow(aug_data)]))/(length(res$residuals))
        
        #aug_data$score[nrow(aug_data)] <- test_data[m,3]
        #res <- lm(score~., data = aug_data)
        #pval <- 1 - (sum(res$residuals < res$residuals[nrow(aug_data)]) + .5*sum(res$residuals == res$residuals[nrow(aug_data)]))/nrow(aug_data)
        #list(p, pval)
        #return(p)
      }
      
      probs <- unlist(prob_data[[1]])
      exp <- test_data$diff > 0
      
      if(by_fave){
        #get in terms of favorite
        check <- probs <= .5
        change_exp <- exp
        change_exp[check] <- 1 - as.numeric(as.logical(change_exp[check]))
        
        probs[check] <- 1 - probs[check]
        exp <- change_exp
      }
      
      #pval <- unlist(prob_data[[2]])
      #all_pval <- rbind(all_pval, cbind(pval, l))
      
      
    }
    
    full_probs[[j]] <- rbind(full_probs[[j]], cbind(probs, exp, l, year_vec[i]))
    
    if(by_fave){
      bin_seq <- seq(.5,1, by = 0.025)
    } else {
      bin_seq <- seq(0,1, by = 0.05)  
    }
    bins <- cut(probs, breaks = bin_seq, labels = FALSE)
    bin_data <- data.frame(bin = bins, probs = probs, exp = exp)
    bin_sum <- bin_data %>% group_by(bin) %>% summarize(mean_prob = mean(probs), sum = sum(exp), count = n())
    bin_probs <- bin_sum %>% mutate(freq = sum/count)
    #get loss...
    
    miss <- (1:(length(bin_seq)-1))[!((1:(length(bin_seq)-1)) %in% bin_probs$bin)]
    if(length(miss) > 0){
      bin_probs <- rbind(bin_probs, data.frame(bin = miss, mean_prob = 0, sum = 0, count = 0, freq = 0))
    }
    bin_probs <- bin_probs %>% arrange(bin)
    
    lECE_year <- bin_probs$count*abs(bin_probs$mean_prob - bin_probs$freq)/sum(bin_probs$count)
    lECE <- sum(lECE_year)
    lMCE <- max(abs(bin_probs$mean_prob - bin_probs$freq))
    lwECE_year <- bin_probs$count*abs(bin_probs$mean_prob - bin_probs$freq)/(bin_probs$mean_prob*(1-bin_probs$mean_prob))/sum(bin_probs$count)
    lwECE_year[is.na(lwECE_year)] <- 0
    lwECE <- sum(lwECE_year)
    
    #print(c(sum(bin_probs$count), i))
    
    #add missing
    
    #yearly
    loss_year[[i]][[j]] <- cbind(lECE, lMCE, lwECE)
    
    #overall
    loss <- rbind(loss, cbind(lECE, lMCE, lwECE, methods[j], l, year_vec[i]))
    
    all_probs <- rbind(all_probs, cbind(bin_probs, l, year_vec[i], methods[j]))
    #all_lECE <- rbind(all_lECE, matrix(c(lECE_year, year_vec[i], methods[j]), nrow = 1, ncol = length(lECE_year) + 1))
    #all_lwECE <- rbind(all_lwECE, matrix(c(lwECE_year, year_vec[i], methods[j]), nrow = 1, ncol = length(lwECE_year) + 1))
  }
} 
}

#may need to adjust TRUE/FALSE
all_bin_probs <- data.frame()

###redo plots###
bin_seq <- seq(0,1, by = 0.05)
full_probs <- readRDS("by_home_probs.RDS")
methods <- c("norm", "BT", "conf")
######
for(k in 1:length(methods)){
  bins <- cut(as.numeric(full_probs[[k]][,1]), breaks = bin_seq, labels = FALSE)
  bin_data <- data.frame(bin = bins, probs = as.numeric(full_probs[[k]][,1]), 
                         exp = as.logical((full_probs[[k]][,2])), 
                         league = full_probs[[k]][,3])
  bin_sum <- bin_data %>% group_by(bin, league) %>% summarize(mean_prob = mean(probs), sum = sum(exp), count = n())
  bin_probs <- bin_sum %>% mutate(freq = sum/count)
  bin_probs$method <- methods[k]
  all_bin_probs <- rbind(all_bin_probs, bin_probs)
}

all_bin_probs <- all_bin_probs %>% 
  mutate(method = factor(method, levels = c("conf", "norm", "BT")))

all_bin_probs <- all_bin_probs %>% filter(!is.na(bin))
loss$lECE <- as.numeric(loss$lECE)
loss$lMCE <- as.numeric(loss$lMCE)
loss$lwECE <- as.numeric(loss$lwECE)
names(loss)[4] = "method"
loss_summary <- loss %>% 
  group_by(method, league) %>% 
  summarize(sum_lECE = sum(lECE), sum_lMCE = mean(lMCE), sum_lwECE = sum(lwECE))

all_bin_probs$method <- factor(all_bin_probs$method, levels = c("conf", "norm", "BT"))

all_bin_probs <- all_bin_probs %>% filter(!is.na(bin))
all_probs <- all_probs %>% filter(!is.na(bin))

names(all_probs) <- c("bin", "mean_prob", "sum", "count", "freq", "league", "year", "method")
all_probs %>% filter(method %in% c("BT", "conf", "norm"), mean_prob > 0, league == "w") %>%
  ggplot(aes(x = mean_prob, y = freq)) + 
  geom_point() +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0) +
  facet_grid(method~year)

all_probs %>% filter(method %in% c("BT", "conf", "norm"), mean_prob > 0, league == "m") %>%
  ggplot(aes(x = mean_prob, y = freq)) + 
  geom_point() +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0) +
  facet_grid(method~year)

all_bin_probs %>% filter(method %in% c("BT", "conf", "norm")) %>%
  ggplot(aes(x = mean_prob, y = freq, color = league)) + 
  geom_point() +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0) +
  facet_wrap(method~., ncol = 2, nrow = 2) +
  theme(axis.title.x=element_blank(),
        legend.position="bottom",
        text = element_text(size = 15))

all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m") %>%
  ggplot(aes(x = mean_prob, y = freq, color = method, shape = method)) + 
  geom_point(size = 2) +
  geom_line(size = 1.05) +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0, size = 1.05)+
  scale_y_continuous(name = "Relative Frequency") +
  scale_x_continuous(name = "Binned Probability Estimate") +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "black", linetype = "dashed"), 
        axis.line = element_line(color = "black")) +
  #scale_shape_discrete(name = "Method", labels = c("Conformal", "Normal", "Logistic")) +
  #scale_color_discrete(name = "Method", labels = c("Conformal", "Normal", "Logistic")) +
  coord_fixed()

all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m", method == "conf") %>%
  ggplot(aes(x = mean_prob, y = freq, color = method, shape = method)) + 
  geom_point(size = 2) +
  geom_line(size = 1.05) +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0, size = 1.05)+
  scale_y_continuous(name = "Relative Frequency") +
  scale_x_continuous(name = "Binned Probability Estimate") +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "black", linetype = "dashed"), 
        axis.line = element_line(color = "black")) +
  #scale_shape_discrete(name = "Method", labels = c("Conformal", "Normal", "Logistic")) +
  #scale_color_discrete(name = "Method", labels = c("Conformal", "Normal", "Logistic")) +
  coord_fixed()

all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w") %>%
  ggplot(aes(x = mean_prob, y = freq, color = method, shape = method)) + 
  geom_point(size = 2) +
  geom_line(size = 1.05) +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0, size = 1.05)+
  scale_y_continuous(name = "Relative Frequency") +
  scale_x_continuous(name = "Binned Probability Estimate") +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "black", linetype = "dashed"), 
        axis.line = element_line(color = "black")) +
  scale_shape_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic")) +
  scale_color_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic")) +
  coord_fixed()

league_labs <- c("Men", "Women")
names(league_labs) <- c("m", "w")

all_bin_probs <- all_bin_probs %>% 
  mutate(league = factor(league, levels = c("w", "m")))

#log_loss_league_season <- log_loss_league_season %>% 
#  mutate(method = factor(method, levels = c("conf", "norm", "BT")))

all_bin_probs %>% filter(method %in% c("BT", "conf", "norm")) %>%
  ggplot(aes(x = mean_prob, y = freq, color = method, shape = method)) + 
  geom_point(size = 2) +
  geom_line(size = 1.05) +
  geom_abline(linetype = "dashed", slope = 1, intercept = 0, size = 1.05)+
  scale_y_continuous(name = "Relative Frequency") +
  scale_x_continuous(name = "Binned Probability Estimate") +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "black", linetype = "dashed"), 
        axis.line = element_line(color = "black"),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA)) +
  scale_shape_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic")) +
  scale_color_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic")) +
  coord_fixed() +
  facet_grid(cols = vars(league), labeller = labeller(league = league_labs))

ggsave("calibration-plot-all-byhome.eps", height = 6)

#all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>% 
#  ggplot(aes(x = mean_prob, y = freq)) + 
#  geom_point() +
#  geom_abline(linetype = "dashed", slope = 1, intercept = 0) +
#  facet_wrap(method~., ncol = 2, nrow = 3) +
#  theme(axis.title.x=element_blank(),
#        legend.position="bottom",
#        text = element_text(size = 15))

#all_bin_probs_loss <- all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  group_by(method, league) %>% 
#  mutate(loss = abs(mean_prob - freq)*count, cumsum=cumsum(loss))

#all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m") %>%
#  ggplot(aes(x = bin, y = loss, group = method, col = method)) +
#  geom_point() + 
#  geom_line()

#all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w") %>%
#  ggplot(aes(x = bin, y = loss, group = method, col = method)) +
#  geom_point() + 
#  geom_line()


#all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m") %>% 
#  ggplot(aes(x = bin, y = cumsum, group = method, col = method)) +
#  geom_point() + 
#  geom_line()

#all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w") %>% 
#  ggplot(aes(x = bin, y = cumsum, group = method, col = method)) +
#  geom_point() + 
#  geom_line()

#all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m") %>% 
#  ggplot(aes(x = bin, y = count, group = method, col = method)) +
#  geom_point() + 
#  geom_line()

all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w") %>% 
  ggplot(aes(x = bin, y = count, group = method, col = method)) +
  geom_point() + 
  geom_line()

all_bin_probs_loss %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w") %>% 
  ggplot(aes(x = bin, y = sum, group = method, col = method)) +
  geom_point() + 
  geom_line()

#loss across years
all_loss <- all_bin_probs %>% filter(method %in% c("BT", "conf", "norm")) %>%
  group_by(method, league) %>%
  summarize(ECE = sum(abs(mean_prob - freq)*count)/sum(count),
            MCE = max(abs(mean_prob - freq)),
            wECE = sum(abs(mean_prob - freq)*count/(mean_prob*(1-mean_prob)))/sum(count))

#all_loss_indiv <- all_bin_probs %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  group_by(method, league) %>%
#  mutate(ECE = abs(mean_prob - freq)*count,
#         wECE = abs(mean_prob - freq)*count/(mean_prob*(1-mean_prob)))

names(loss)[4:6] <- c("method", "league", "year")
loss_mutate <- loss %>% group_by(year, league) %>%
  mutate(lECE_norm = lECE/min(lECE), lMCE_norm = lMCE/min(lMCE), lwECE_norm = lwECE/min(lwECE))

#loss_mutate %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  ggplot(aes(y = lECE_norm, x = year, group = method, color = method)) +
#  geom_point() + 
#  geom_line()

#loss_mutate %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  ggplot(aes(y = lMCE_norm, x = year, group = method, color = method)) +
#  geom_point() + 
#  geom_line()

#all_loss_indiv %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  ggplot(aes(y = ECE, x = bin, group = method, color = method)) +
#  geom_point() + 
#  geom_line()

#all_loss_indiv %>% filter(method %in% c("BT", "conf", "norm", "seq")) %>%
#  ggplot(aes(y = wECE, x = bin, group = method, color = method)) +
#  geom_point() + 
#  geom_line()
#melted <- melt(wp)
#plot3d(melted$Var1, melted$Var2, melted$value)

#melted <- melt(iso_wp)
#plot3d(melted$Var1, melted$Var2, melted$value)

hold_mean <- matrix(all_bin_probs$mean_prob, nrow = 20)
hold_freq <- matrix(all_bin_probs$freq, nrow = 20)

#plot(x = hold_mean[,1], y = hold_freq[,1], pch = 20, lty = 1)
w <- 2
h <- 2
par(pin=c(w, h))
par(mfrow = c(2,2))
for (i in 1:length(methods)){
  plot(x = hold_mean[,i], y = hold_freq[,i], pch = 20, lty = 1)
  abline(a = 0, b = 1, lty = 2)
  #lines(x = hold_mean[,i], y = hold_freq[,i], pch = i+20)
}

all_pval <- data.frame(all_pval)
all_m <- all_pval %>% filter(l == "m")
all_w <- all_pval %>% filter(l == "w")

#hist(as.numeric(all_m$pval))
#hist(as.numeric(all_w$pval))

loss_mutate_filt_m <- loss_mutate %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "m")
loss_mutate_filt_w <- loss_mutate %>% filter(method %in% c("BT", "conf", "norm", "seq"), league == "w")
loss_mutate_filt <- rbind(loss_mutate_filt_m, loss_mutate_filt_w)
#write.csv(loss_mutate_filt, "calibration_loss_compare_conformal_fix.csv")
#write.csv(all_loss, "all_calibration_loss_compare_conformal_fix.csv")
#write.csv(loss_mutate_filt, "calibration_loss_compare_conformal_fix2.csv")
#write.csv(all_loss, "all_calibration_loss_compare_conformal_fix2.csv")

#check
all_bin_probs %>% group_by(method) %>% summarize(s = sum(count))
hist(as.numeric(full_probs[[1]][,1]), main = "Normal")
hist(as.numeric(full_probs[[2]][,1]), main = "Logistic")
hist(as.numeric(full_probs[[3]][,1]), main = "Conformal")

probs_mat <- exp_mat <- matrix(0, nrow = nrow(full_probs[[1]]), ncol = 3)

probs_mat[,1] <- as.numeric(full_probs[[1]][,1])
probs_mat[,2] <- as.numeric(full_probs[[2]][,1])
probs_mat[,3] <- as.numeric(full_probs[[3]][,1])

#exp_mat[,1] <- as.numeric(full_probs[[1]][,2])
full_probs <- readRDS("by_home_probs.RDS")

full_probs_numeric <- list()
full_probs_numeric[[1]] <- full_probs[[1]]
full_probs_numeric[[2]] <- full_probs[[2]]
full_probs_numeric[[3]] <- full_probs[[3]]

full_probs_numeric[[1]][,1] <- as.numeric(full_probs_numeric[[1]][,1])
full_probs_numeric[[2]][,1] <- as.numeric(full_probs_numeric[[2]][,1])
full_probs_numeric[[3]][,1] <- as.numeric(full_probs_numeric[[3]][,1])
full_probs_numeric[[1]][,2] <- as.numeric(as.logical(full_probs_numeric[[1]][,2]))
full_probs_numeric[[2]][,2] <- as.numeric(as.logical(full_probs_numeric[[2]][,2]))
full_probs_numeric[[3]][,2] <- as.numeric(as.logical(full_probs_numeric[[3]][,2]))

full_probs_numeric[[1]] <- data.frame(full_probs_numeric[[1]], method = methods[1])
full_probs_numeric[[2]] <- data.frame(full_probs_numeric[[2]], method = methods[2])
full_probs_numeric[[3]] <- data.frame(full_probs_numeric[[3]], method = methods[3])

full_probs_df <- as.data.frame(rbind(full_probs_numeric[[1]], full_probs_numeric[[2]], full_probs_numeric[[3]]))
full_probs_mat <- as.data.frame(cbind(full_probs_numeric[[1]][,1], 
                                      full_probs_numeric[[2]][,1], 
                                      full_probs_numeric[[3]][,1]))
full_probs_df$probs <- as.numeric(full_probs_df$probs)
full_probs_df$exp <- as.numeric(full_probs_df$exp)

log_loss_all <- full_probs_df %>% 
  group_by(method) %>% 
  mutate(log_loss = -(exp*log(probs) + (1-exp)*log(1-probs))) %>%
  summarize(total = sum(log_loss)) %>%
  #group_by(l, V4) %>% 
  mutate(norm_total = total/min(total))

log_loss_league <- full_probs_df %>% 
  group_by(method, l) %>% 
  mutate(log_loss = -(exp*log(probs) + (1-exp)*log(1-probs))) %>%
  summarize(total = sum(log_loss)) %>%
  group_by(l) %>% 
  mutate(norm_total = total/min(total))

log_loss_season <- full_probs_df %>% 
  group_by(method, V4) %>% 
  mutate(log_loss = -(exp*log(probs) + (1-exp)*log(1-probs))) %>%
  summarize(total = sum(log_loss))%>%
  group_by(V4) %>% 
  mutate(norm_total = total/min(total))

log_loss_league_season <- full_probs_df %>% 
  group_by(method, l, V4) %>% 
  mutate(log_loss = -(exp*log(probs) + (1-exp)*log(1-probs))) %>%
  summarize(total = sum(log_loss)) %>%
  group_by(l, V4) %>% 
  mutate(norm_total = total/min(total))

#15 135 255 375
gg_color_hue <- function() {
  hues = c(15, 255, 135)
  hcl(h = hues, l = 65, c = 100)[1:3]
}

#maybe change relative to conformal
#log_loss_league_season$V4 <- ordered(log_loss_league_season$V4, levels = as.character(seq(2014:2020)))
log_loss_league_season <- log_loss_league_season %>% 
  mutate(method = factor(method, levels = c("conf", "norm", "BT")))
log_loss_league_season <- log_loss_league_season %>% 
  mutate(l = factor(l, levels = c("w", "m")))
log_loss_league_season %>% filter(method %in% c("conf", "norm", "BT")) %>%
  ggplot(aes(x = as.factor(V4), y = norm_total, color = method, shape = method, group = method)) + 
  geom_point(size = 3.5) +
  geom_line(size = 1.05) +
  scale_y_continuous(name = "Relative Log-Loss") +
  scale_x_discrete(name = "Season") +
  #facet_wrap(l,1,2, scales = "free", labeller = labeller(l = league_labs)) +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "black", linetype = "dashed"),
        panel.spacing = unit(1.5, "lines"), 
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA)) +
  facet_grid(cols = vars(l), scales = "free", labeller = labeller(l = league_labs)) +
  scale_shape_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic")) +
  scale_color_discrete(name = "Method", labels = c("Conformal", "Linear", "Logistic"))

ggsave("log-loss-season-league-byhome-v2.eps", height = 5)

#full_probs_df %>% ggplot(aes())

h1 <- table(cut(as.numeric(full_probs_mat[,1]), breaks = seq(.5,1,by = .025), labels = FALSE))
h2 <- table(cut(as.numeric(full_probs_mat[,2]), breaks = seq(.5,1,by = .025), labels = FALSE))
h3 <- table(cut(as.numeric(full_probs_mat[,3]), breaks = seq(.5,1,by = .025), labels = FALSE))
plot(c(h1), pch = 20)
points(c(h2), pch = 20, col = "red")
points(c(h3), pch = 20, col = "blue")

all_bin_probs_loss %>% group_by(bin, method) %>% summarize(s = sum(count))
all_bin_probs_loss %>% group_by(method) %>% summarize(s = sum(count))

if(by_fave){
  saveRDS(full_probs, "by_fave_probs.RDS")
} else {
  saveRDS(full_probs, "by_home_probs.RDS")
}

all_probs_mutate <- all_probs %>% group_by(year, league, method, bin) %>%
  mutate(binECE = count*abs(mean_prob - freq), 
         binwECE = count*abs(mean_prob - freq)/(mean_prob*(1-mean_prob)))
