

crop_list <- c("barley", "cassava", "groundnut", "maize", "millet", "potato", "rice", "sorghum", "soybean", "sugarbeet", "sugarcane", "wheat")
library(dplyr)
library(tidyverse)
library(doParallel)
library(doMPI)

##### Functions #####

# divides the cells to train and test from different 60 arcmin / 1 degree boxes
degree_sampler_function <- function(bin_data, iteration_number) {
  
  #get coordinate values at one degree accuracy
  x_list <- unique(floor(bin_data$x))
  y_list <- unique(floor(bin_data$y))
  
  #create a matrix for all combinations of coordinates, assign to train and test
  set.seed(123+iteration_number)
  test_matrix <- matrix(sample(c("train", "test"), length(y_list)*length(x_list), replace = TRUE, prob = c(0.75, 0.25)), ncol = length(x_list))
  sampler_frame <- as.data.frame(test_matrix)
  #give names
  colnames(sampler_frame) <- x_list
  rownames(sampler_frame) <- y_list
  
  #create a column to store the division to train and test
  bin_data$divide <- "NA"
  
  #for all coordinate combinations, assign row to train or test set based on sampler matrix
  for (a in x_list) {
    for (b in y_list) {
      
      bin_data[floor(bin_data$x) == a & floor(bin_data$y) == b , names(bin_data) %in% c("divide")] <- sampler_frame[as.character(b),as.character(a)]
      
    }
  }
  return(bin_data)
}


#creates a shock scenario and predicts the yield with the forest
scenario_function <- function(bin_forest, scenario_input_df, result_df, scenario_percent) {
  
  #different shock scenarios and combinations
  scenario_list <- list('N_rate', 'P_rate', 'K_rate', 'machinery', c('pesticide1', 'pesticide2', 'pesticide3', 'pesticidesum17'), 
                        c('N_rate', 'P_rate', 'K_rate'),
                        c('N_rate', 'P_rate', 'K_rate', 'machinery', 'pesticide1', 'pesticide2', 'pesticide3', 'pesticidesum17'))
  
  #add column for scenario percent info
  result_df$scenario_percent <- (1 - scenario_percent) * 100
  
  
  for (group in scenario_list) {
    scenario_group <- unlist(group)
    scenario_input <- scenario_input_df
    
    # choose inputs to modify in scenario
    if (length(scenario_group) == 1) {
      scenario_input[[scenario_group]] <- unlist(lapply(scenario_input[[scenario_group]], function(x) x*scenario_percent))
      
    } else {
      scenario_input[ , scenario_group] <- lapply(scenario_input[ , scenario_group], function(x) x*scenario_percent)
    }
    # predict new yields based on scenario
    scenario_input$prediction <- predict(bin_forest, scenario_input, type = "response")
    
    scenario_input_to_results <- scenario_input %>%
      dplyr::select(cell, next_results = prediction)
    
    # add prediction results to 
    result_df <- result_df %>%
      left_join(scenario_input_to_results, by = 'cell')
    
    if (length(scenario_group) == 8) {
      #change column name to the scenario treatment
      names(result_df)[names(result_df) == 'next_results'] <- "shock_all"
    } else if (length(scenario_group) == 3) {
      names(result_df)[names(result_df) == 'next_results'] <- "shock_fert_only"
    } else if (length(scenario_group) == 4) {
      names(result_df)[names(result_df) == 'next_results'] <- "pesticide_shock"
    } else {
      names(result_df)[names(result_df) == 'next_results'] <- paste0(scenario_group, "_shock")
    }
    
  }
  return(result_df) 
  
}


# makes one random forest model and predicts scenarios with it


one_iteration <-  function(bin_data, y) {
  
  #divide/sample data in 60arcmin/1degree slots
  divided_data <- degree_sampler_function(bin_data, y)
  
  bin_train <- divided_data %>%
    dplyr::filter(divide == "train") %>%
    dplyr::select(-divide)
  
  bin_test <- divided_data %>%
    dplyr::filter(divide == "test" ) %>%
    dplyr::select(-divide)
  
  # train forest
  bin.rf <- randomForest(ES_yield_A ~ N_rate + P_rate + K_rate +
                           machinery + workers + pesticide1 + pesticide2 + pesticide3 + pesticidesum17 +
                           soil_org_carbon + soil_P + soil_N + nonmineral_N + nonmineral_P + irrigation_share,
                         data = bin_train,
                         ntree=1000, mtry = 5, nodesize = 5)
  
  results_out <- list()
  
  #save forest variables to dataframe
  bin.df <- data.frame(matrix(nrow = 1, ncol = 5))
  colnames(bin.df) <- c("iteration" ,"rmse", "rsq", "RMSE", "NSE")
  
  bin.df$rmse <- sqrt(tail(bin.rf$mse,1))
  bin.df$rsq <- tail(bin.rf$rsq,1)
  
  #save iteration info
  bin.df$iteration <- y
  
  #validation of forest
  bin_test$predicted <- predict(bin.rf, bin_test, type = "response")
  
  #save forest validation results to dataframe
  bin.df$RMSE <- sqrt(mean((bin_test$ES_yield_A - bin_test$predicted)^2))
  bin.df$NSE <- NSE(as.matrix(bin_test$predicted), as.matrix(bin_test$ES_yield_A))
  
  results_out$bindf <- bin.df
  
  #save validation yields to dataframe with original yields
  obs_vs_pred_iteration <- bin_test %>%
    dplyr::select(cell, observed = ES_yield_A, model_yield = predicted) %>%
    dplyr::mutate(iteration = y)
  
  results_out$obs_vs_pred <- obs_vs_pred_iteration
  
  ##### SCENARIOS #####
  #predict scenario yields here
  scenario_input_data <- bin_data %>%
    dplyr::select(cell, ES_yield_A, N_rate, P_rate, K_rate, machinery, workers, pesticide1, pesticide2, pesticide3, pesticidesum17,
                  soil_org_carbon, soil_P, soil_N, nonmineral_N, nonmineral_P, irrigation_share)
  
  # dataframe to store original yield and scenario yields
  scenario_yields <- bin_data %>%
    dplyr::select(cell, x, y, observed_normal_yield = ES_yield_A)
  
  #predict new yields with forest
  scenario_yields_25 <- scenario_function(bin.rf, scenario_input_data, scenario_yields, 0.25)
  
  scenario_yields_50 <- scenario_function(bin.rf, scenario_input_data, scenario_yields, 0.5)
  
  scenario_yields_75 <- scenario_function(bin.rf, scenario_input_data, scenario_yields, 0.75)
  
  scenario_yields_all <- bind_rows(scenario_yields_25, scenario_yields_50)
  scenario_yields_all <- bind_rows(scenario_yields_all, scenario_yields_75)
  
  #save column for iteration
  scenario_yields_all$iteration <- y
  
  # add to masterdataframe
  results_out$bin_scenarios <- scenario_yields_all
  
  
  #make predictor from forest to make pdp/ale figures
  
  if (length(unique(bin_train$irrigation_share)) == 1) { # no ALE can be done if irrigation only has one value in bin
    ale_df <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(ale_df) <- c("ale_value", "input_rate", "input_name", "iteration")
    ale_df$input_name <- as.character(ale_df$input_name)
    ale_data_iteration <- ale_df
    
  } else {
    model_variables <- bin_train %>%
      dplyr::select(-cell, -x, -y, -ES_yield_A) 
    
    rf.predictor <- Predictor$new(bin.rf, data = model_variables, 
                                  y = bin_train$ES_yield_A)
    
    ale_data_iteration <- extract_ale(rf.predictor, y) 
  }
  
  results_out$aledf <- ale_data_iteration
  
  return(results_out)
  
}



extract_ale <- function(forest_predictor, iteration_y) {
  
  ale_effects <- FeatureEffects$new(forest_predictor, method = "ale")
  ale_results <- ale_effects$results
  ale_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(ale_df) <- c("ale_value", "input_rate", "input_name", "iteration")
  ale_df$input_name <- as.character(ale_df$input_name)
  
  for (input in c("N_rate", "P_rate", "K_rate", "machinery", "workers", "pesticide1", "pesticide2", "pesticide3",
                  "pesticidesum17", "soil_org_carbon", "soil_N", "soil_P", "nonmineral_N", "nonmineral_P", "irrigation_share")){
    
    input_ale <- ale_results[[input]]
    
    input_ale <- input_ale %>%
      dplyr::rename(ale_value = .value, input_rate = .borders, input_name = .feature) %>%
      dplyr::mutate(iteration = iteration_y) %>%
      dplyr::select(-.type)
    
    ale_df <- bind_rows(ale_df, input_ale)
    
  }
  
  return(ale_df)
  
}


##### Random forests for all crops #####


path_to_data <- "revision2_dfs/"

path_to_save <- "results_revision2/"

#choose number of forest iterations per bin
iterations <- 1

##### CROP #####

for (i in 1) {
  
  file_extension <- "_revised2_model_df.RData"
  
  #retrieve dataframe
  file_name <- paste0(path_to_data, crop_list[i], file_extension)
  load(file_name)
  
  crop_df[is.na(crop_df)] <- 0
  
  #dataframe to store performance score means
  crop_means.df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(crop_means.df) <- c("rmse", "rsq", "RMSE", "NSE", "bin")
  
  #dataframe to store performance score variances
  crop_vars.df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(crop_vars.df) <- c("rmse", "rsq", "RMSE", "NSE", "bin")
  
  #dataframe to store performance score standard deviations
  crop_sds.df <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(crop_sds.df) <- c("rmse", "rsq", "RMSE", "NSE", "bin")
  
  # aggregate 100 bins to 25 super bins
  #crop_df_aggregated <- aggregator_function(crop_df)
  
  
  ##### BIN #####
  # for each (super) bin
  for (z in 1:25) {
    
    # select bin data, delete rows where yield is 0
    bin_crop <- crop_df %>%
      dplyr::filter(bin == z, ES_yield_A!= 0)
    
    ##### CHECK ADDED DATA #####
    #select data to be used in model
    bin_data <- bin_crop %>%
      dplyr::select(cell, x, y, ES_yield_A, N_rate, P_rate, K_rate, machinery, workers, pesticide1, pesticide2,
                    pesticide3, pesticidesum17, soil_org_carbon, soil_P, soil_N, nonmineral_N, nonmineral_P, irrigation_share)
    
    #dataframe to store model performance results
    bin.df <- data.frame(matrix(nrow = iterations, ncol = 5))
    colnames(bin.df) <- c("iteration" ,"rmse", "rsq", "RMSE", "NSE")
    
    #save forest validation yields to dataframe
    obs_vs_pred <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(obs_vs_pred) <- c("cell", "observed", "model_yield", "iteration")
    
    #dataframe to store scenario results
    bin_scenarios <- data.frame(matrix(nrow = 0, ncol = 13))
    colnames(bin_scenarios) <- c("cell", "x", "y", "observed_normal_yield",
                                 "scenario_percent",  "N_rate_shock",
                                 "P_rate_shock", "K_rate_shock", 
                                 "machinery_shock",
                                 "pesticide_shock",
                                 "shock_fert_only", "shock_all", "iteration")
    
    # dataframe to store ale-results
    bin_ale_df <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(bin_ale_df) <- c("ale_value", "input_rate", 
                              "input_name", "iteration")
    bin_ale_df$input_name <- as.character(bin_ale_df$input_name)
    
    ##### ITERATION #####
    
    #setup parallel computing
    # n_cores <- detectCores() - 1
    # cluster <- makeCluster(n_cores)
    # registerDoParallel(cluster)
    cl <- startMPIcluster()
    registerDoMPI(cl)
    
    # create many forests with different sampling arrangement
    parallel_results <- foreach(y= 1:iterations, .packages = c("dplyr", "tidyverse", "randomForest", "hydroGOF", "iml")) %dopar% one_iteration(bin_data, y)
    
    closeCluster(cl)
    #stopCluster(cluster)
    
    # extract data from parallel computing results
    for (iteration in 1:iterations) {
      
      iteration_results <- parallel_results[[iteration]]
      
      bin.df[iteration, ] <- iteration_results$bindf
      
      obs_vs_pred <- bind_rows(obs_vs_pred, iteration_results$obs_vs_pred)
      
      bin_scenarios <- bind_rows(bin_scenarios, iteration_results$bin_scenarios)
      
      bin_ale_df <- bind_rows(bin_ale_df, iteration_results$aledf)
      
    }
    
    
    # save observed vs predicted yield dataframe
    save_name <- paste0(path_to_save, "validation/", crop_list[i],"_bin", z, "_validation.RData")
    save(obs_vs_pred, file = save_name)
    
    save_perf_scores <- paste0(path_to_save, "performances/", crop_list[i],"_climatebin", z, "_perf_scores.RData")
    save(bin.df, file = save_perf_scores)
    
    #add results from bin to summary tables
    binsummary.df <- bin.df %>%
      dplyr::summarise(across(everything(), mean))
    binsummary.df$bin <- z
    
    crop_means.df <- bind_rows(crop_means.df, binsummary.df)
    
    
    variances.df <- bin.df %>%
      dplyr::summarise(across(everything(), var))
    variances.df$bin <- z
    
    crop_vars.df <- bind_rows(crop_vars.df, variances.df)
    
    stdevs.df <- bin.df %>%
      dplyr::summarise(across(everything(), sd))
    stdevs.df$bin <- z
    
    crop_sds.df <- bind_rows(crop_sds.df, stdevs.df)
    
    #save bin scenarios
    save_bin_scenarios <- paste0(path_to_save, "scenarios/", crop_list[i],"_climatebin", z, "_scenarios.RData")
    save(bin_scenarios, file = save_bin_scenarios)
    
    #save bin ale df
    save_bin_ale <- paste0(path_to_save, "ALEs/", crop_list[i],"_climatebin", z, "_aledf.RData")
    save(bin_ale_df, file = save_bin_ale)
    
    rm(bin_scenarios)
    rm(obs_vs_pred)
    
  }
  
  #save crop summaries
  save_means <- paste0(path_to_save, "summaries/", crop_list[i], "_means.RData")
  save(crop_means.df, file = save_means)
  
  save_vars <- paste0(path_to_save, "summaries/", crop_list[i], "_variances.RData")
  save(crop_vars.df, file = save_vars)
  
  save_stdevs <- paste0(path_to_save, "summaries/", crop_list[i], "_stdevs.RData")
  save(crop_sds.df, file = save_stdevs)
  
}
