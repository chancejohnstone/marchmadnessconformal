## ---------------------------
##
## Script name: all_year_ranks.R
##
## Purpose of script:
##
## Author: Chancellor Johnstone
##
## Date Created: 2021-01-28
##
## Copyright (c) Chancellor Johnstone, 2021
## Email: cjohnsto@iastate.edu
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
library(MASS)
library(doParallel)
library(parallel)

dir <- getwd()
source(paste0(dir,"/sim_tournament.R"))
source(paste0(dir,"/weekly_res_gen.R"))

#ncaa_data <- read.csv("clean_ncaab_scores_2014_2020_all.csv")
#gender <- "m"
#neutral <- TRUE

#ncaa_data <- read.csv("clean_ncaaw_scores_2014_2020_all.csv")
#gender <- "w"
#neutral <- FALSE

ncaa_data <- read.csv("clean_ncaam_current.csv")
ncaa_data <- read.csv("clean_ncaaw_current.csv")
gender <- "m"
neutral <- FALSE

year_vec <- min(ncaa_data$year):max(ncaa_data$year)

j <- 20
for(i in 1:length(year_vec)){
   data <- ncaa_data[ncaa_data$year == year_vec[i],]
   
   home_count <- table(data$HomeTeam)
   away_count <- table(data$AwayTeam)
   
   home_check_team <- names(home_count)[home_count > 5]
   away_check_team <- names(away_count)[away_count > 5]
   
   all <- unique(c(unique(data$AwayTeam), unique(data$HomeTeam)))
   
   keep_home <- match(all, home_check_team)
   keep_away <- match(all, away_check_team)
   keep <- all[apply(!is.na(cbind(keep_home, keep_away)), FUN = sum, MARGIN = 1) == 2]
   
   small_data <- data[apply(!is.na(cbind(match(data$AwayTeam, keep), match(data$HomeTeam, keep))), 
                            FUN = sum, 
                            MARGIN = 1) == 2,]
   
   keep_id <- seq(1,nrow(small_data))[small_data$conf_tourn == FALSE]
   train_data <- small_data[keep_id,]
   test_data <- small_data[-keep_id,]
   
   id <- seq(1,nrow(train_data))
   week <- cut(id, 30, labels = FALSE)
   train_data$week <- week
   
   #model
   final_res <- weekly_res_gen(train_data, 
                               train_data[1,], 
                               neutral = neutral)
   
   file_name <- paste0("final_model_", gender, "_", year_vec[i], ".RDS")
   saveRDS(final_res$fit, file_name)
   
   #clean training data for everything else
   #file_name <- paste0("ncaa_train_data_m_", year_vec[i], "_clean.RDS")
   #saveRDS(train_data, file_name)
   
   #clean training data for BT model
   file_name <- paste0("ncaa_train_data_", gender, "_", year_vec[i], ".RDS")
   saveRDS(final_res$train_data, file_name)
   
   #test data for probs (not as matrix)
   file_name <- paste0("ncaa_test_data_", gender, "_", year_vec[i], ".RDS")
   saveRDS(test_data[,1:4], file_name)
   
   #test data for probs (as matrix)
   test_data$week <- NA
   file_name <- paste0("ncaa_test_matrix_data_", gender, "_", year_vec[i], ".RDS")
   final_res_dummy <- weekly_res_gen(train_data, 
                                     test_data, 
                                     neutral = neutral)
   saveRDS(final_res_dummy$test_data, file_name)
   
   #ranks
   ranks <- final_res$fit$coefficients
   ranks <- c(ranks, 0)
   
   #sometime Abilene Christian is not in the data...
   names(ranks)[length(ranks)] <- sort(keep)[1]
   ranks <- c(ranks, -10000)
   names(ranks)[length(ranks)] <- "BYE"
   ranks <- ranks[names(ranks) != "int"]
   
   ranks <- ranks[order(ranks, decreasing = TRUE)]
   ranks <- data.frame(strength = ranks)
   
   file_name <- paste0("ncaa_ranks_" , gender, "_", year_vec[i], "_sort.csv")
   write.csv(ranks, file_name)
}
