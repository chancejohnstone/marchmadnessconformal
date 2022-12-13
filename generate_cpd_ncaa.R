library(dplyr)
library(ggplot2)
library(Iso)
library(doParallel)
library(parallel)

#combine funcion
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

#rotate function
rotate <- function(x) t(apply(x, 2, rev))

#generate CPD for a specific year, league, game
generate_cpd <- function(year, 
                         league, 
                         home_team, 
                         away_team, 
                         range = c(-50,50), 
                         num_cand = 201,
                         cand = NULL,
                         tau = 1/2,
                         plot = FALSE){
  year_vec <- c(year)
  leagues <- league
  
  #put all cpd requests into df form
  all_request <- data.frame(year, leagues, home_team, away_team)
  
  #always performal with conformal approach
  methods <- c("conf")
  
  loss_year <- rep( list(list()), length(year_vec))
  loss <- data.frame()
  all_lECE <- all_lwECE <- all_probs <- c()
  full_probs <- rep( list(c()), length(methods))
  all_pval <- c()
  track <- 1
  cpd <- c()
  
  for(k in 1:nrow(all_request)){
    #get year and league
    i <- k
    l <- all_request$leagues[k]

    file_name_rank <- paste0("ncaa_ranks_", l, "_", year_vec[i], "_sort.csv")
    ranks_all <- read.csv(file_name_rank)
    ranks_all[,1] <- gsub("`","",ranks_all[,1])
    
    #check to make sure teams have been ranked
    if(!(home_team[i] %in% ranks_all[,1])){
      print(paste(home_team[i], "is not a valid team for CPD generation. Try another team."))
      break
    } 
    
    if(!(away_team[i] %in% ranks_all[,1])){
      print(paste(away_team[i], "is not a valid team for CPD generation. Try another team."))
      break
    } 
      
    file_name_train <- paste0("ncaa_train_data_", l, "_", year_vec[i], ".RDS")
    file_name_test <- paste0("ncaa_test_data_", l, "_", year_vec[i], ".RDS")
    file_name_test_mat <- paste0("ncaa_test_matrix_data_", l, "_", year_vec[i], ".RDS")
    train_data <- readRDS(file_name_train)
    test_data <- readRDS(file_name_test)
    
    #need to create test data matrix
    test_data_mat <- readRDS(file_name_test_mat)
    for(j in 1:length(methods)){
     
      home <- match(test_data$HomeTeam, ranks_all[,1])
      away <- match(test_data$AwayTeam, ranks_all[,1])
      all_comb_sort <- cbind(away, home)
      
      #estimated and empirical
      exp <- test_data$diff > 0
      
      #fix this
      #parallel
      ###in parallel
      num_threads <- 8
      if(is.null(num_threads)){
        num_threads <- detectCores()
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
      test_data_vec <- data.frame(matrix(c(1,1,rep(0, times = ncol(test_data_mat) - 2)), ncol = ncol(test_data_mat)))
      names(test_data_vec) <- names(test_data_mat)
      
      test_data_vec[home_team[i]] <- 1
      test_data_vec[away_team[i]] <- -1 
      
      if(is.null(cand)){
        y_cand <- seq(range[1], range[2], length.out = num_cand)
      } else {
        y_cand <- cand
      }
  
      #generate different cpd values in parallel
      prob_data <- foreach(m = 1:length(y_cand)) %dopar% {
        aug_data <- rbind(train_data, test_data_vec)
        aug_data$score[nrow(aug_data)] <- y_cand[m]
        
        res <- lm(score~., data = aug_data)
        pval <- (sum(res$residuals < res$residuals[nrow(aug_data)]) + tau*sum(res$residuals == res$residuals[nrow(aug_data)]))/nrow(aug_data)
      }
        
      probs <- unlist(prob_data)
      
    }
    
    cpd <- cbind(cpd, probs)

  }
  
  cpd <- data.frame(cpd)
  
  df_args <- c(all_request, sep="")
  do.call(paste, df_args)
  names(cpd) <- do.call(paste, df_args)
  
  if(plot == TRUE){
    matplot(cpd, type = "l")
  }
  
  return(list(cpd = cpd, cand = y_cand))
}
