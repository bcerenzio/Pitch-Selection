
library(tidyverse)
library(arrow)
library(xgboost)
library(dplyr)
library(fastDummies)
library(rsample)

statcast_21_24_ordered <- read_parquet('~/Downloads/Aidan_final_statcast_21_24_ordered.parquet')

contact_xg <- statcast_21_24_ordered %>%    #choosing columns for model
  reframe(contact,
          plate_x_in, 
          plate_z_in, 
          pitch_type, 
          release_speed,
          accel_x = ifelse(p_throws == "L", accel_x * -1, accel_x), 
          release_spin_rate, 
          spin_axis, 
          arm_angle,
          release_pos_x_in = ifelse(p_throws == "L", release_pos_x_in * -1, release_pos_x_in),
          release_pos_z_in, 
          release_extension_in, 
          previous_pitch,
          accel_x_diff = ifelse(p_throws == "L", accel_x_diff * -1, accel_x_diff),
          accel_z_diff,
          plate_x_in_diff = ifelse(stand == "L", plate_x_in_diff * -1, plate_x_in_diff),
          plate_z_in_diff, 
          same_hand, 
          three_balls,
          contact_pct, 
          pitch_contact_pct, 
          two_strike, 
          attack_zone) %>% drop_na(contact)


contact_xg <- contact_xg %>%    #automatically setting attack zone as dummies
  dummy_cols(select_columns = c('attack_zone', 'pitch_type', 'previous_pitch'), remove_selected_columns = TRUE)

#contact_xg <- contact_xg %>% select(-contact)

set.seed(101); split <- initial_split(contact_xg, strata = contact) #splitting data

train_data <- training(split)  #make training data
dtrain <- xgb.DMatrix(as.matrix(train_data %>% select(-contact)), label = train_data$contact)

test_data <- testing(split)    #make testing data
dtest <- xgb.DMatrix(as.matrix(test_data %>% select(-contact)), label = test_data$contact)

hyperparam_ps_tuning_reg <- function(max_depth_ps, weight_ps, subsample_ps, row_num_ps){
  
  print(paste('Max Depth: ', max_depth_ps))
  print(paste('Weight: ', weight_ps))
  print(paste('Subsample: ', subsample_ps))
  print(paste('Row Number: ', row_num_ps))
  
  set.seed(101);mod_pitching <- xgb.train(
    params = list(
      eta = 0.5,
      objective = 'binary:logistic',
      eval_metric = 'logloss',
      gamma = 1,
      max_depth = max_depth_ps,
      min_child_weight = weight_ps,
      subsample = subsample_ps
      #tree_method = 'approx',
      #grow_policy = 'lossguide'
    ),
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, test = dtest),
    print_every_n = 50,
    early_stopping_rounds = 10,
    nthread = 7
  ) 
  logloss <- mod_pitching$evaluation_log %>% 
    slice_min(test_logloss, n = 1) %>% 
    pull(test_logloss)
  
  return(logloss)
  
}

reg_tuning_ps_df <- expand_grid(
  max_depth = c(3,6,8),
  weight = c(1, 4, 10),
  subsample = c(0.5, 0.75, 1)
) %>% 
  mutate(row_num = row_number())

reg_tuning_ps_df <- reg_tuning_ps_df %>% 
  rowwise() %>% 
  mutate(
    logloss = pmap_dbl(list(max_depth, weight, subsample, row_num), hyperparam_ps_tuning_reg)
  ) %>% 
  ungroup()

# A tibble: 5 × 5
#max_depth weight subsample row_num logloss
#<dbl>  <dbl>     <dbl>   <int>   <dbl>
# 6      4         1      15   0.433
# 6     10         1      18   0.434
# 6      1         1      12   0.434
# 3     10         1       9   0.434
# 3      1         1       3   0.434

reg_tuning_ps_df %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_ps_best <- reg_tuning_ps_df %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

max_depth_ps <- reg_tuning_ps_best$max_depth # 6
weight_ps <- reg_tuning_ps_best$weight # 4
subsample_ps <- reg_tuning_ps_best$subsample # 1


dcontact <- xgb.DMatrix(as.matrix(contact_xg %>% select(-contact)), label = contact_xg$contact)



set.seed(101);contact_mod <- xgboost(
  params = list(
    eta = 0.5,
    objective = 'binary:logistic',
    eval_metric = 'logloss',
    gamma = 1,
    max_depth = max_depth_ps,
    min_child_weight = weight_ps,
    subsample = subsample_ps
  ),
  data = dcontact,
  nrounds = 500,
  print_every_n = 50,
  early_stopping_rounds = 5,
  nthread = 7
) 

statcast_21_24_ordered$contact_probability <- predict(contact_mod, 
                                                      as.matrix(statcast_21_24_ordered %>%    
                                                        reframe(
                                                                plate_x_in, 
                                                                plate_z_in, 
                                                                pitch_type, 
                                                                release_speed,
                                                                accel_x = ifelse(p_throws == "L", accel_x * -1, accel_x), 
                                                                release_spin_rate, 
                                                                spin_axis, 
                                                                arm_angle,
                                                                release_pos_x_in = ifelse(p_throws == "L", release_pos_x_in * -1, release_pos_x_in),
                                                                release_pos_z_in, 
                                                                release_extension_in, 
                                                                previous_pitch,
                                                                accel_x_diff = ifelse(p_throws == "L", accel_x_diff * -1, accel_x_diff),
                                                                accel_z_diff,
                                                                plate_x_in_diff = ifelse(stand == "L", plate_x_in_diff * -1, plate_x_in_diff),
                                                                plate_z_in_diff, 
                                                                same_hand, 
                                                                three_balls,
                                                                contact_pct, 
                                                                pitch_contact_pct, 
                                                                two_strike, 
                                                                attack_zone) %>% 
                                                           #automatically setting attack zone as dummies
                                                        dummy_cols(select_columns = c('attack_zone', 'pitch_type', 'previous_pitch'), remove_selected_columns = TRUE)
                                                        )
                                                      )

write_parquet(statcast_21_24_ordered, 'final_statcast_21_24_ordered.parquet')

