library(arrow)
library(tidyverse)
library(tidymodels)
library(usemodels)
library(textrecipes)
library(doParallel)
library(ranger)
library(explore)
library(finetune)
library(vip)

statcast_21_24_ordered <- read_parquet("final_statcast_21_24_ordered.parquet")
balls_in_play <- subset(statcast_21_24_ordered, statcast_21_24_ordered$description == "hit_into_play")
balls_in_play <- balls_in_play %>% select(xwOBA, stand, p_throws, plate_x_in, plate_z_in, attack_zone, pitch_type, release_speed,
                                            accel_x, accel_z, release_spin_rate, spin_axis, arm_angle, 
                                            release_pos_x, release_pos_z, release_extension, previous_pitch,
                                            accel_x_diff, accel_z_diff, plate_x_in_diff, plate_z_in_diff, 
                                            season_woba, season_woba_pitchtype, same_hand, two_strike, three_balls, Park_Factor)
balls_in_play <- na.omit(balls_in_play)

balls_in_play$plate_x_in <- ifelse(balls_in_play$stand == "L", balls_in_play$plate_x_in*-1, balls_in_play$plate_x_in)
balls_in_play$plate_x_in_diff <- ifelse(balls_in_play$stand == "L", balls_in_play$plate_x_in_diff *-1, balls_in_play$plate_x_in_diff)
balls_in_play$accel_x <- ifelse(balls_in_play$p_throws == "L", balls_in_play$accel_x*-1, balls_in_play$accel_x)
balls_in_play$accel_x_diff <- ifelse(balls_in_play$p_throws == "L", balls_in_play$accel_x_diff *-1, balls_in_play$accel_x_diff)
balls_in_play$release_pos_x <- ifelse(balls_in_play$p_throws == "L", balls_in_play$release_pos_x*-1, balls_in_play$release_pos_x)

set.seed(101);bb_split <- initial_split(balls_in_play)

bb_train <- training(bb_split)
bb_test <- testing(bb_split)

bb_rec <-
  recipe(xwOBA ~ plate_x_in + plate_z_in + attack_zone + pitch_type + release_speed +
           accel_x + accel_z + release_spin_rate + spin_axis + arm_angle + 
           release_pos_x + release_pos_z + release_extension + previous_pitch +
           accel_x_diff + accel_z_diff + plate_x_in_diff + plate_z_in_diff + 
           season_woba + season_woba_pitchtype + same_hand + two_strike + 
           three_balls + Park_Factor
           ,
         data = bb_train
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_grid <- grid_space_filling(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), bb_train),
  learn_rate(),
  size = 30
)


xgb_wf <- workflow() %>%
  add_recipe(bb_rec) %>%
  add_model(xgb_spec)

doParallel::registerDoParallel()

set.seed(123)
bb_folds <- vfold_cv(bb_train, v = 3, strata = xwOBA)
bb_folds

set.seed(234)
xgb_res <- tune_race_anova(
  xgb_wf,
  resamples = bb_folds,
  grid = 5,
  metrics = metric_set(rmse),
  control = control_race(verbose_elim = TRUE, burn_in = 2)
)

plot_race(xgb_res)
show_best(xgb_res)

xgb_last <- xgb_wf %>%
  finalize_workflow(select_best(xgb_res)) %>%
  last_fit(bb_split)


extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15)


predictions <- predict(extract_workflow(xgb_last), new_data = statcast_21_24_ordered)

statcast_21_24_ordered$xdamage <- predictions$.pred

write_parquet(statcast_21_24_ordered, 'final_statcast_21_24_ordered.parquet')
