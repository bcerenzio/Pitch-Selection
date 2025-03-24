library(tidyverse)
library(fastDummies)
library(xgboost)
library(rsample)
library(arrow)

slice <- dplyr::slice

setwd("~/Desktop/Syracuse/Semesters/Spring 2025/Arizona/Pitch Selection")

statcast_21_24_ordered <- read_parquet('Data/final_statcast_21_24_ordered.parquet')

swing_finaldf <- statcast_21_24_ordered %>% 
  reframe(swing, pitch_type, previous_pitch, release_speed, pitch_velo_diff, 
          release_pos_x_in = ifelse(p_throws == 'L', -release_pos_x_in, release_pos_x_in), release_pos_z_in , 
      accel_x = ifelse(p_throws == 'L', -accel_x, accel_x), 
      accel_x_diff = ifelse(p_throws == 'L', -accel_x_diff, accel_x_diff), 
      accel_z , accel_z_diff , 
      plate_x_in = ifelse(stand == 'L', -plate_x_in, plate_x_in), 
      plate_x_in_diff = ifelse(stand == 'L', -plate_x_in_diff, plate_x_in_diff), plate_z_in , plate_z_in_diff , attack_zone ,
      release_spin_rate , release_extension_in , spin_axis , arm_angle , three_balls , two_strike , 
      pitch_swing_pct , pitch_zswing_pct , pitch_chase_pct , swing_pct , zswing_pct , chase_pct , same_hand)


set.seed(101);swing_split <- initial_split(swing_finaldf, prop = 0.65, strata = swing)

glimpse(swing_finaldf)

train_swing <- training(swing_split)
test_swing <- testing(swing_split)

dtrain_swing <- xgb.DMatrix(as.matrix(train_swing %>%
                                        dummy_cols(select_columns = c('pitch_type', 'previous_pitch', 'attack_zone'), remove_selected_columns = TRUE) %>% 
                                        select(-swing)), label = train_swing$swing)


dtest_swing <- xgb.DMatrix(as.matrix(test_swing %>%
                                       dummy_cols(select_columns = c('pitch_type', 'previous_pitch', 'attack_zone'), remove_selected_columns = TRUE) %>% 
                                       select(-swing)), label = test_swing$swing)


hyperparam_swing_tuning_reg <- function(max_depth, weight,subsample, row_num){
  
  print(paste('Max Depth: ', max_depth))
  print(paste('Weight: ', weight))
  print(paste('Subsample: ', subsample))
  print(paste('Row Number: ', row_num))
  
  set.seed(101); mod <- xgb.train(
    params = list(
      eta = 0.5,
      objective = 'binary:logistic',
      eval_metric = 'logloss', 
      max_depth = max_depth,
      min_child_weight = weight,
      subsample = subsample),
    data = dtrain_swing,
    watchlist = list(train = dtrain_swing, test = dtest_swing),
    nrounds = 1000,
    print_every_n = 10,
    early_stopping_rounds = 10,
    nthread = 7
  ) 
  
  logloss <- mod$evaluation_log %>% 
    slice_min(test_logloss, n = 1) %>% 
    pull(test_logloss)
  
  return(logloss)
  
}

reg_tuning_swing_df <- expand_grid(
  max_depth = c(3,6,8),
  weight = c(1,4,12),
  subsample = c(0.5,0.75,1)
) %>% 
  mutate(row_num = row_number())


reg_tuning_swing_df <- reg_tuning_swing_df %>% 
  rowwise() %>% 
  mutate(
    logloss = pmap_dbl(list(max_depth, weight, subsample, row_num), hyperparam_swing_tuning_reg)
  ) %>% 
  ungroup()

#run lines from here
reg_tuning_swing_df %>% 
  arrange(logloss) %>% 
  head(5)

reg_tuning_swing_best <- reg_tuning_swing_df %>% 
  slice_min(logloss, n = 1) %>% 
  dplyr::slice(1)

max_depth_swing <- reg_tuning_swing_best$max_depth
weight_swing <- reg_tuning_swing_best$weight 
subsample_swing <- reg_tuning_swing_best$subsample 

swing_finaldf <- swing_finaldf %>% dummy_cols(select_columns = c('pitch_type', 'previous_pitch', 'attack_zone'), remove_selected_columns = TRUE)

d_final_swing <- xgb.DMatrix(as.matrix(swing_finaldf %>% select(-swing)), label = swing_finaldf$swing)

set.seed(101); swing_model <- xgboost(
  params = list(
    eta = 0.5,
    objective = 'binary:logistic',
    eval_metric = 'logloss', 
    max_depth = max_depth_swing,
    min_child_weight = weight_swing,
    subsample = subsample_swing),
  data = d_final_swing,
  nrounds = 1000,
  print_every_n = 10,
  early_stopping_rounds = 10,
  nthread = 7
)

save(swing_model, file = 'swing_model.RData')

statcast_21_24_ordered$swing_probability <- predict(swing_model, as.matrix(statcast_21_24_ordered %>% 
                            reframe(swing, pitch_type, previous_pitch, release_speed, pitch_velo_diff,
                                    release_pos_x_in = ifelse(p_throws == 'L', -release_pos_x_in, release_pos_x_in), release_pos_z_in , 
                                    accel_x = ifelse(p_throws == 'L', -accel_x, accel_x), 
                                    accel_x_diff = ifelse(p_throws == 'L', -accel_x_diff, accel_x_diff), 
                                    accel_z , accel_z_diff , 
                                    plate_x_in = ifelse(stand == 'L', -plate_x_in, plate_x_in), 
                                    plate_x_in_diff = ifelse(stand == 'L', -plate_x_in_diff, plate_x_in_diff), plate_z_in , plate_z_in_diff , attack_zone ,
                                    release_spin_rate , release_extension_in , spin_axis , arm_angle , three_balls , two_strike , 
                                    pitch_swing_pct , pitch_zswing_pct , pitch_chase_pct , swing_pct , zswing_pct , chase_pct , same_hand) %>% 
                            dummy_cols(select_columns = c('pitch_type', 'previous_pitch', 'attack_zone'), remove_selected_columns = TRUE) %>% 
                              select(-swing)))


write_parquet(statcast_21_24_ordered, 'final_statcast_21_24_ordered.parquet')




