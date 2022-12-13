get_wp_oos <- function(){
  model_summary <- summary(model)
  cov_mat <- model_summary$cov.unscaled
  
  all_comb <- t(combn(1:(nrow(ranks_all)-1), 2))
  all_comb_flip <- cbind(all_comb[,2], all_comb[,1])
  
  new <- matrix(0, nrow = nrow(all_comb), ncol = ncol(cov_mat))
  for(i in 1:nrow(all_comb)){
    new[i,all_comb[i,1]] <- 1
    new[i, all_comb[i,2]] <- -1
  }
  new[,1] <- rep(0, times = nrow(all_comb))
  new_df <- data.frame(new)
  
  #issues with parantheses
  names(new_df) <- names(model$coefficients)
  names(new_df) <- gsub("`","",names(new_df))
  
  #alphabetical preds
  preds <- predict(model, new_df)
  
  #get scale constant
  hat <- rep(0, times = nrow(all_comb))
  for(i in 1:nrow(all_comb)){
    hat[i] <- sqrt(1 + t(as.matrix(new[i,])) %*% cov_mat %*% as.matrix(new[i,]))
  }
  
  #ecdf of residuals
  fold_ecdf <- ecdf(errors - median(errors))
  probs <- fold_ecdf(preds/hat)
  
  coeff <- names(model$coefficients)
  names_all_comb <- matrix(coeff[c(all_comb)], nrow = nrow(all_comb), ncol = 2)
  names_all_comb[names_all_comb == "int"] <- "AbileneChristian"
  
  #issues with parantheses
  names_all_comb <- gsub("`","",names_all_comb)
  #names_all_comb <- gsub("&","",names_all_comb)
  #names_all_comb <- gsub("'","",names_all_comb)
  
  ranks_all[,1] <- gsub("`","",ranks_all[,1])
  
  home <- match(names_all_comb[,1], ranks_all[,1])
  away <- match(names_all_comb[,2], ranks_all[,1])
  
  all_comb_sort <- cbind(home, away)
  all_comb_sort_flip <- cbind(away, home)
  
  #win probability
  wp <- matrix(0, nrow = nrow(ranks_all), ncol = nrow(ranks_all))
  wp[all_comb_sort] <- probs
  wp[all_comb_sort_flip] <- 1-probs
  diag(wp) <- .5
  n <- nrow(wp)
  wp[,n] <- 1
  wp[n,] <- 0
  wp[n,n] <- .5
  contour(wp, levels = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
  
  return(wp)
}


#all possible game combinations{
get_wp_norm <- function(){
  model_summary <- summary(model)
  cov_mat <- model_summary$cov.unscaled
  
  all_comb <- t(combn(1:(nrow(ranks_all)-1), 2))
  all_comb_flip <- cbind(all_comb[,2], all_comb[,1])
  
  new <- matrix(0, nrow = nrow(all_comb), ncol = ncol(cov_mat))
  for(i in 1:nrow(all_comb)){
    new[i,all_comb[i,1]] <- 1
    new[i, all_comb[i,2]] <- -1
  }
  new[,1] <- rep(0, times = nrow(all_comb))
  new_df <- data.frame(new)
  
  #issues with parantheses
  names(new_df) <- names(model$coefficients)
  names(new_df) <- gsub("`","",names(new_df))
  
  #alphabetical preds
  preds <- predict(model, new_df)
  
  #get scale constant
  hat <- rep(0, times = nrow(all_comb))
  for(i in 1:nrow(all_comb)){
    hat[i] <- sqrt(1 + t(as.matrix(new[i,])) %*% cov_mat %*% as.matrix(new[i,]))
  }
  
  #t-distributed probs
  #probs <- pt(preds/(model_summary$sigma*hat), model$df.residual)
  probs <- pnorm(preds/(model_summary$sigma*hat))
  
  coeff <- names(model$coefficients)
  names_all_comb <- matrix(coeff[c(all_comb)], nrow = nrow(all_comb), ncol = 2)
  
  ranks_all[,1] <- gsub("`","",ranks_all[,1])
  
  names_all_comb[names_all_comb == "int"] <- sort(ranks_all[,1])[1]
  
  #issues with parantheses
  names_all_comb <- gsub("`","",names_all_comb)
  
  home <- match(names_all_comb[,1], ranks_all[,1])
  away <- match(names_all_comb[,2], ranks_all[,1])
  
  all_comb_sort <- cbind(home, away)
  all_comb_sort_flip <- cbind(away, home)
  
  #win probability
  wp <- matrix(0, nrow = nrow(ranks_all), ncol = nrow(ranks_all))
  wp[all_comb_sort] <- probs
  wp[all_comb_sort_flip] <- 1-probs
  diag(wp) <- .5
  n <- nrow(wp)
  wp[,n] <- 1
  wp[n,] <- 0
  wp[n,n] <- .5
  contour(wp, levels = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
  
  return(wp)
}



#all possible game combinations{
get_wp_BT <- function(){
  model_summary <- summary(model)
  cov_mat <- model_summary$cov.unscaled
  
  all_comb <- t(combn(1:(nrow(ranks_all)-1), 2))
  all_comb_flip <- cbind(all_comb[,2], all_comb[,1])
  
  new <- matrix(0, nrow = nrow(all_comb), ncol = ncol(cov_mat))
  for(i in 1:nrow(all_comb)){
    new[i,all_comb[i,1]] <- 1
    new[i, all_comb[i,2]] <- -1
  }
  new[,1] <- rep(0, times = nrow(all_comb))
  new_df <- data.frame(new)
  
  #issues with parantheses
  names(new_df) <- names(model$coefficients)
  names(new_df) <- gsub("`","",names(new_df))
  ranks_all[,1] <- gsub("`","",ranks_all[,1])
  
  #alphabetical preds
  preds <- predict(model, new_df)
  
  #get scale constant
  hat <- rep(0, times = nrow(all_comb))
  for(i in 1:nrow(all_comb)){
    hat[i] <- sqrt(1 + t(as.matrix(new[i,])) %*% cov_mat %*% as.matrix(new[i,]))
  }
  
  #BT model
  small_data <- data.frame(win = as.numeric(data$diff > 0), diff = predict(model))
  bt_res <- glm(win~0+diff, data = small_data, family = binomial("logit"))
  preds_dat <- data.frame(preds)
  names(preds_dat) <- "diff"
  probs <- predict(bt_res, preds_dat, type = "response")
  
  coeff <- names(model$coefficients)
  names_all_comb <- matrix(coeff[c(all_comb)], nrow = nrow(all_comb), ncol = 2)
  names_all_comb[names_all_comb == "int"] <- sort(ranks_all[,1])[1]
  
  #issues with parantheses
  names_all_comb <- gsub("`","",names_all_comb)
  
  home <- match(names_all_comb[,1], ranks_all[,1])
  away <- match(names_all_comb[,2], ranks_all[,1])
  
  all_comb_sort <- cbind(home, away)
  all_comb_sort_flip <- cbind(away, home)
  
  #win probability
  wp <- matrix(0, nrow = nrow(ranks_all), ncol = nrow(ranks_all))
  wp[all_comb_sort] <- probs
  wp[all_comb_sort_flip] <- 1-probs
  diag(wp) <- .5
  n <- nrow(wp)
  wp[,n] <- 1
  wp[n,] <- 0
  wp[n,n] <- .5
  contour(wp, levels = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
  
  return(wp)
}
######

######

######
leagues <- c("m","w")
year_vec <- as.character(seq(2014, 2020))
#year_vec <- as.character(2020)
for(i in 1:length(year_vec)){
  #normality-based
  for(l in leagues){
    file_name_train <- paste0("ncaa_train_data_", l, "_", year_vec[i], ".RDS")
    file_name_rank <- paste0("ncaa_ranks_", l, "_", year_vec[i], "_sort.csv")
    file_name_model <- paste0("final_model_", l, "_", year_vec[i], ".RDS")
    wp_list <- list()
    ranks_all <- read.csv(file_name_rank)
      
    #final model
    model <- readRDS(file_name_model)
    
    list_name <- paste0("wp_",l)
    wp_list[[list_name]] <- get_wp_norm()
    
    file_name_wp <- paste0("norm_wp_", l, "_", year_vec[i], ".RDS")
    saveRDS(wp_list, file_name_wp)
    
  }
######

######
  #Bradley-Terry
  for(l in leagues){
    file_name_train <- paste0("ncaa_train_data_", l, "_", year_vec[i], ".RDS")
    file_name_rank <- paste0("ncaa_ranks_", l, "_", year_vec[i], "_sort.csv")
    file_name_model <- paste0("final_model_", l, "_", year_vec[i], ".RDS")
    wp_list <- list()
    ranks_all <- read.csv(file_name_rank)
    data <- readRDS(file_name_train)
    names(data)[1] <- "diff"
      
    #final model
    model <- readRDS(file_name_model)
    
    list_name <- paste0("wp_",l)
    wp_list[[list_name]] <- get_wp_BT()
    
    file_name_wp <- paste0("BT_wp_", l, "_", year_vec[i], ".RDS")
    saveRDS(wp_list, file_name_wp)
    
  }
}
######

w <- 3.5
h <- 3.5
par(pin=c(w, h), cex = 1.1)
contour(x = 1:349, y = 1:349, z = wp, levels = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), labcex = 1.01, lwd = 2,
        ylim = c(1,349),
        xlim = c(1,349),
        ylab = "Away Team",
        xlab = "Home Team",
        xaxt = "n",
        yaxt = "n")
axis(1, at = c(1,50, 100, 150, 200, 250, 300))
axis(2, at = c(1,50, 100, 150, 200, 250, 300))
