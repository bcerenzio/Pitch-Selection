#install.packages('arrow')
library(tidyverse)
library(arrow)


#### Joining Park Factors df to statcast ####
# statcast_21_24 <- statcast_21_24 %>% 
#   left_join(park_factors, by = c('home_team' = 'Team', 'game_year' = 'Year'))
# 
# glimpse(statcast_21_24)

#### Putting dataset in order ####
statcast_21_24_ordered <- statcast_21_24 %>% 
  arrange(game_pk, game_date, at_bat_number, pitch_number)

remove(statcast_21_24) #save space
#### Calculating Run Expecantancy by Count (by PA) ####
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(pitch_count = str_c(balls, strikes, sep = '-')) %>% 
  relocate(pitch_count, .after = 'strikes')

#finding wOBA constants via Fangraphs
woba_2024 <- .310
wobascale_2024 <- 1.242
woba_2023 <- .318
wobascale_2023 <- 1.204
woba_2022 <- .310
wobascale_2022 <- 1.259
woba_2021 <- .314
wobascale_2021 <- 1.209

run_exp_fun <- function(game_year, woba_val){
  if(game_year == 2021){
    run_exp <- (woba_val - woba_2021)/wobascale_2021
  } else if(game_year == 2022){
    run_exp <- (woba_val - woba_2022)/wobascale_2022
  } else if (game_year == 2023){
    run_exp <-  (woba_val - woba_2023)/wobascale_2023
  } else if (game_year == 2024){
    run_exp <- (woba_val - woba_2024)/wobascale_2024
  }
  
  return(run_exp)
}


#finding the final pitch in an AB and seeing if any runs scored
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  rowwise() %>% 
  mutate(run_exp = ifelse(pitch_number == max(pitch_number) & woba_denom == 1, run_exp_fun(game_year, woba_value), NA)) %>% 
  ungroup()


### filling in NA values
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  fill(run_exp, .direction = 'up')

# finding run expectancy in each count by year
run_exp_matrix <- statcast_21_24_ordered %>% 
  filter(balls <= 3, strikes <= 2) %>% #removing any data errors
  group_by(game_year, pitch_count) %>% 
  reframe(run_exp = mean(run_exp, na.rm = TRUE),
          n = n())

# scaling to a 0-0 count
run_exp_matrix <- run_exp_matrix %>% 
  group_by(game_year) %>% 
  mutate(first_pitch_run_exp = ifelse(pitch_count == '0-0', run_exp, NA)) %>% 
  fill(first_pitch_run_exp, .direction = 'down') %>% 
  ungroup() %>% 
  mutate(count_delta_run_exp = run_exp - first_pitch_run_exp) %>% 
  select(-first_pitch_run_exp)


# joining delta run expectancies to current count
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(run_exp_matrix %>% select(-run_exp, -n), by = c('game_year','pitch_count'))

# moving count_delta_run_exp next to pitch_count
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  relocate(count_delta_run_exp, .after = 'pitch_count')

# removing 4 ball and 3 strike "counts"
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  filter(strikes <= 2, balls <= 3)

#### Finding Heart, Shadow, Chase, & Waste portions of the zone (Manipulating plate_x_in In this block of code) ####

# converting plate location to inches
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(plate_x_in = plate_x*12,
         plate_z_in = plate_z * 12) %>% 
  relocate(plate_x_in, .after = plate_x) %>% 
  relocate(plate_z_in, .after = plate_z)

# converting top and bottom of strikezone coordinates to inches
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(sz_top_in = sz_top*12,
         sz_bot_in = sz_bot * 12) %>% 
  relocate(sz_top_in, .after = sz_top) %>% 
  relocate(sz_bot_in, .after = sz_bot)


#### Renaming xwOBACON column ####
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  rename('xwOBA' = estimated_woba_using_speedangle)

#### Removing any ABS where pitches didn't track
original_num_rows <- nrow(statcast_21_24_ordered)
# removing NA pitch locations
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  filter(!any(is.na(plate_x))) %>% 
  ungroup()

# ensuring no plate_z NAs are present
any(is.na(statcast_21_24_ordered$plate_z)) #FALSE

unique(statcast_21_24_ordered$pitch_type)

# removing PAs with NA pitch types, pitchouts, "Other" pitch type, & Eephuses
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  filter(!any(is.na(pitch_type)), !any(pitch_type %in% c('EP','FA','PO'))) %>% 
  ungroup()

# combining slow curves with curvballs
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(pitch_type = ifelse(pitch_type == 'CS', 'CU', pitch_type))

# how much data was lost
cat('% of Data Remaining: ', nrow(statcast_21_24_ordered)/original_num_rows)

summary(statcast_21_24_ordered) # checking for any more NAs for imputation
# NAs by variable:
# spin axis: 11899
# release_pos_y: 175
# release_extension: 3757
# effective_speed: 4167
# release_spin_rate: 11903
# pfx_x: 10
#pfx_z
# release_speed: 2
# release_pos_x: 175
# release_pos_z: 175
#armangle: 38900

#imputing means for pitchers based on year, and pitch type
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(pitcher, pitch_type, game_year) %>% 
  mutate(across(c(spin_axis, release_speed, effective_speed, release_pos_y, release_spin_rate,
                  pfx_x, pfx_z,release_pos_x, release_pos_z, arm_angle), ~replace_na(., mean(., na.rm = TRUE)))) %>% 
  ungroup()

# imputing NA release extenstion through release_pos_y
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(release_extension = ifelse(is.na(release_extension), 60.5-release_pos_y, release_extension))

summary(statcast_21_24_ordered) # still some NAs in the previous values (minimal though)


any(is.na(statcast_21_24_ordered$release_extension)) #FALSE

#replacing remaining of NA values with mean values across 2021-2024
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(pitcher, pitch_type) %>% 
  mutate(across(c(spin_axis, release_speed, effective_speed, release_pos_y, release_spin_rate,
                  pfx_x, release_pos_x, release_pos_z, arm_angle), ~replace_na(., mean(., na.rm = TRUE)))) %>% 
  ungroup()

summary(statcast_21_24_ordered) # still some NAs in the previous values
# NA Values
# release_spin_rate = 1
# spin_axis = 1
#arm_angle = 45
### All are only relevant to the current pitch, remove After finding lagged/lead values ####

#calculating time to plate
mph_to_ftps <- 1.466666667
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(time_to_plate = release_pos_y/(release_speed*mph_to_ftps)) %>% 
  relocate(time_to_plate, .after = 'release_speed')

summary(statcast_21_24_ordered$time_to_plate)

#finding accel_x & accel_z (in/s^2)
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(accel_x = pfx_x/(time_to_plate^2),
         accel_z = pfx_z/(time_to_plate^2)
         ) %>% 
  mutate(accel_x = accel_x*12,
         accel_z = accel_z*12) %>% 
  relocate(accel_x, .after = 'pfx_x') %>% 
  relocate(accel_z, .after = 'pfx_z')

#finding release position in inches
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(release_pos_x_in = release_pos_x*12,
         release_pos_z_in = release_pos_z*12,
         release_pos_y_in = release_pos_y*12,
         release_extension_in = release_extension*12) %>% 
  relocate(release_pos_x_in, .after = 'release_pos_x') %>% 
  relocate(release_pos_z_in, .after = 'release_pos_z') %>% 
  relocate(release_pos_y_in, .after = 'release_pos_y') %>% 
  relocate(release_extension_in, .after = 'release_extension')

# finding information from previous pitch
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  group_by(game_pk, at_bat_number) %>% 
  mutate(previous_pitch = ifelse(is.na(lag(pitch_type)), 'first_pitch', lag(pitch_type)),
         pitch_velo_diff = ifelse(is.na(lag(release_speed)), 0, release_speed -lag(release_speed)),
         plate_x_in_diff = ifelse(is.na(lag(plate_x_in)), 0, plate_x_in - lag(plate_x_in)),
         plate_z_in_diff = ifelse(is.na(lag(plate_z_in)), 0, plate_z_in - lag(plate_z_in)),
         accel_x_diff = ifelse(is.na(lag(accel_x)), 0, accel_x - lag(accel_x)),
         accel_z_diff = ifelse(is.na(lag(accel_z)), 0, accel_z - lag(accel_z))) %>% 
  ungroup() %>% 
  relocate(previous_pitch, .after = 'pitch_type') %>% 
  relocate(pitch_velo_diff, .after = 'release_speed') %>% 
  relocate(plate_x_in_diff, .after = 'plate_x_in') %>% 
  relocate(plate_z_in_diff, .after = 'plate_z_in') %>% 
  relocate(accel_x_diff, .after = 'accel_x') %>% 
  relocate(accel_z_diff, .after = 'accel_z')

## Dummy for 2 and 3 ball counts 
statcast_21_24_ordered <- statcast_21_24_ordered %>%
  mutate(two_strike = ifelse(strikes == 2, 1, 0)) %>%
  mutate(three_balls = ifelse(balls == 3, 1, 0))

## If pitcher and batter handedness is the same
statcast_21_24_ordered <- statcast_21_24_ordered %>%
  mutate(same_hand = ifelse(p_throws == stand, 1, 0))

# new_count column (hypothetical count if batter took the pitch)
statcast_21_24_ordered <- statcast_21_24_ordered %>%
  mutate(new_count = case_when(
    zone > 9 ~ paste(balls + 1, strikes, sep = "-"),
    zone <= 9 ~ paste(balls, strikes + 1, sep = "-")
  )) %>% 
  relocate(new_count, .after = 'pitch_count')

# post-count
statcast_21_24_ordered <- statcast_21_24_ordered %>%
  group_by(game_pk, at_bat_number) %>% 
  mutate(post_pitch_count = lead(pitch_count)) %>%
  ungroup() %>% 
  relocate(post_pitch_count, .after = 'new_count')

# filling in NAs in post count (ie where the at bat ended)
# finding out what the count would've been if the at-bat continued
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(post_pitch_count = ifelse(is.na(post_pitch_count),
                                   case_when(
                                     events %in% c('walk','hit_by_pitch') ~ str_c(balls + 1, strikes, sep = '-'),
                                     !(events %in% c('walk','hit_by_pitch','truncated_pa')) ~ str_c(balls, strikes+1, sep = '-'),
                                     events == 'truncated_pa' & zone > 9 ~ str_c(balls + 1, strikes, sep = '-'),
                                     events == 'truncated_pa' & zone <= 9 ~ str_c(balls, strikes+1, sep = '-')
                                   ), post_pitch_count))
  
# wOBA constants for wBB
woba_BB_2024 <- .689
woba_BB_2023 <- .696
woba_BB_2022 <- .689
woba_BB_2021 <- .692

#joining delta run expectancies to hypothetical and next pitch count
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(run_exp_matrix %>% reframe(game_year, pitch_count, new_count_delta_run_exp = count_delta_run_exp), 
            by = c('game_year','new_count' = 'pitch_count')) %>% 
  left_join(run_exp_matrix %>% reframe(game_year, pitch_count, post_count_delta_run_exp = count_delta_run_exp), 
by = c('game_year','post_pitch_count' = 'pitch_count'))

#relocating them next to each other
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  relocate(new_count_delta_run_exp, .after = 'new_count') %>% 
  relocate(post_count_delta_run_exp, .after = 'post_pitch_count')

# adding run value for walk events
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(new_count_delta_run_exp = ifelse(
    is.na(new_count_delta_run_exp) & new_count %in% c('4-0','4-1','4-2'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,woba_BB_2021),
      game_year == 2022 ~ run_exp_fun(2022,woba_BB_2022),
      game_year == 2023 ~ run_exp_fun(2023,woba_BB_2023),
      game_year == 2024 ~ run_exp_fun(2024,woba_BB_2024),
    ), new_count_delta_run_exp
      ),
    post_count_delta_run_exp = ifelse(
      is.na(post_count_delta_run_exp) & post_pitch_count %in% c('4-0','4-1','4-2'),
      case_when(
        game_year == 2021 ~ run_exp_fun(2021,woba_BB_2021),
        game_year == 2022 ~ run_exp_fun(2022,woba_BB_2022),
        game_year == 2023 ~ run_exp_fun(2023,woba_BB_2023),
        game_year == 2024 ~ run_exp_fun(2024,woba_BB_2024),
      ), post_count_delta_run_exp
    )  
    )

# adding run value for a strikeout
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(new_count_delta_run_exp = ifelse(
    is.na(new_count_delta_run_exp) & new_count %in% c('0-3','1-3','2-3','3-3'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,0),
      game_year == 2022 ~ run_exp_fun(2022,0),
      game_year == 2023 ~ run_exp_fun(2023,0),
      game_year == 2024 ~ run_exp_fun(2024,0),
    ), new_count_delta_run_exp
  ),
  post_count_delta_run_exp = ifelse(
    is.na(post_count_delta_run_exp) & post_pitch_count %in% c('0-3','1-3','2-3','3-3'),
    case_when(
      game_year == 2021 ~ run_exp_fun(2021,0),
      game_year == 2022 ~ run_exp_fun(2022,0),
      game_year == 2023 ~ run_exp_fun(2023,0),
      game_year == 2024 ~ run_exp_fun(2024,0),
    ), post_count_delta_run_exp
  )  
  )

#identifying swings
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(swing = ifelse(description %in% c('foul','foul_tip','hit_into_play','swinging_strike',
                                           'swinging_strike_blocked','foul_bunt','missed_bunt','bunt_foul_tip'), 1,0),
         zswing = ifelse(zone <= 9, swing/(zone<= 9), NA),
         chase = ifelse(zone > 9, swing/(zone > 9), NA)) %>% 
  relocate(swing, .after = batter) %>% 
  relocate(zswing, .after = swing) %>% 
  relocate(chase, .after = zswing)

# batter swing%
batter_swing <- statcast_21_24_ordered %>% 
  group_by(batter, player_name) %>% 
  reframe(swing_pct = mean(swing, na.rm = TRUE),
          zswing_pct = mean(zswing, na.rm = TRUE),
          chase_pct = mean(chase, na.rm = TRUE),
          pitches = n()) %>% 
  mutate(across(c('swing_pct','zswing_pct'), 
                ~ifelse(pitches <= 100, quantile(., probs = 0.1, na.rm = TRUE), .))) %>% 
  mutate(chase_pct = ifelse(pitches <= 100, quantile(chase_pct, probs = 0.9, na.rm = TRUE), chase_pct))

# swing% by pitch 
batter_swing_pitch <- statcast_21_24_ordered %>% 
  group_by(batter, player_name, pitch_type) %>% 
  reframe(pitch_swing_pct = mean(swing, na.rm = TRUE),
          pitch_zswing_pct = mean(zswing, na.rm = TRUE),
          pitch_chase_pct = mean(chase, na.rm = TRUE),
          pitches = n()) %>% 
  group_by(pitch_type) %>% 
  mutate(across(c('pitch_swing_pct','pitch_zswing_pct'), 
                ~ifelse(pitches <= 40, quantile(., probs = 0.1, na.rm = TRUE), .))) %>% 
  mutate(pitch_chase_pct = ifelse(pitches <= 40, quantile(pitch_chase_pct, probs = 0.9, na.rm = TRUE), pitch_chase_pct)) %>% 
  ungroup()

summary(batter_swing_pitch$pitches)

statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(batter_swing_pitch %>% select(-'player_name', -'pitches'), by = c('batter','pitch_type')) %>% 
  left_join(batter_swing %>% select(-'player_name', -'pitches'), by = c('batter'))

#identifying contanct
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(contact = case_when(
    swing == 1 & description %in% c('foul', 'foul_tip', 'hit_into_play',
                                    'foul_bunt','bunt_foul_tip') ~ 1,
    swing == 1 & !(description %in% c('foul', 'foul_tip', 'hit_into_play',
                                    'foul_bunt','bunt_foul_tip')) ~ 0,
    .default = NA
  )) %>% 
  relocate(contact, .after = 'chase')

batter_contact_pct <- statcast_21_24_ordered %>% 
  group_by(batter, player_name) %>% 
  reframe(contact_pct = mean(contact, na.rm = TRUE),
          pitches = n()) %>% 
  mutate(contact_pct = ifelse(pitches <= 100, quantile(contact_pct, probs = 0.1, na.rm = TRUE), contact_pct))

batter_contact_pct_pitch <- statcast_21_24_ordered %>% 
  group_by(batter, player_name, pitch_type) %>% 
  reframe(pitch_contact_pct = mean(contact, na.rm = TRUE),
          pitches = n()) %>% 
  group_by(pitch_type) %>% 
  mutate(pitch_contact_pct = ifelse(pitches <= 40, quantile(pitch_contact_pct, probs = 0.1, na.rm = TRUE), pitch_contact_pct))

statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(batter_contact_pct %>% select(-pitches, -player_name), by = 'batter') %>% 
  left_join(batter_contact_pct_pitch %>% select(-pitches, -player_name), by = c('batter','pitch_type'))
  

# batter season woba
batter_woba <- statcast_21_24_ordered %>% 
  group_by(batter, player_name) %>% 
  reframe(woba = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
          pa = sum(woba_denom, na.rm = TRUE))

summary(batter_woba$pa)

# batter woba imputation
batter_woba <- batter_woba %>% 
  mutate(woba = ifelse(pa <= 50, quantile(woba, probs = 0.1), woba))

#batter woba by pitch type
batter_woba_pitchtype <- statcast_21_24_ordered %>% 
  group_by(batter, player_name, pitch_type) %>% 
  reframe(woba_pitch = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
          pa = sum(woba_denom, na.rm = TRUE))

summary(batter_woba_pitchtype$pa)

batter_woba_pitchtype <- batter_woba_pitchtype %>% 
  group_by(pitch_type) %>% 
  mutate(woba_pitch = ifelse(is.infinite(woba_pitch), quantile(woba_pitch, probs = 0.6, na.rm= TRUE), woba_pitch),
         woba_pitch = ifelse(pa < 20, quantile(woba_pitch, probs = 0.1,  na.rm = TRUE), woba_pitch)) %>% 
  ungroup()

statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(batter_woba %>% reframe(batter, season_woba = woba), by = 'batter') %>% 
  left_join(batter_woba_pitchtype %>% reframe(batter, pitch_type, season_woba_pitchtype = woba_pitch), by = c('batter','pitch_type'))


#### Identifying Attack Zones Code ####
#adding  left and right positions for attack zones
heart_left <- -6.7; heart_right <- 6.7
shadow_farleft <- -13.3; shadow_left <- -6.7
shadow_farright <- 13.3; shadow_right <- 6.7
chase_farleft <- (-13.3-6.6); chase_left <- -13.3
chase_farright <- (13.3+6.6);chase_right <- 13.3
waste_left <- -13.3-6.6; waste_right <- 13.3+6.6


# finding middle of vertical strikezone
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(sz_mid_in = (sz_top_in + sz_bot_in)/2) %>% 
  relocate(sz_mid_in, .after = sz_top_in)

# finding "percentages: in pitch location vs strike zone from attack zones
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(plate_z_pct = ifelse(plate_z_in <= sz_mid_in,(plate_z_in - sz_mid_in)/(sz_mid_in-sz_bot_in)*100, (plate_z_in - sz_mid_in)/(sz_top_in-sz_mid_in)*100)) %>% 
  relocate(plate_z_pct, .after = plate_z_in) %>% 
  relocate(sz_mid_in, .after = plate_z_pct)

# adding percentage positions for attack zones
heart_pct_bot <- -67; heart_pct_top <- 67
shadow_pct_farbot <- -133; shadow_pct_bot <- -67
shadow_pct_fartop <- 133; shadow_pct_top <- 67
chase_pct_farbot <- -200; chase_pct_bot <- -133
chase_pct_fartop <- 200; chase_pct_top <- 133
waste_bot <- -200; waste_top <- 200

# finding attack zones
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(attack_zone = case_when(
    (plate_x_in <= heart_right & plate_x_in >= heart_left) &
      (plate_z_pct <= heart_pct_top & plate_z_pct >= heart_pct_bot) ~ 'Heart',
    (plate_x_in <= shadow_farright & plate_x_in > shadow_right & 
      plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
      plate_z_pct <= shadow_pct_fartop & plate_z_pct > shadow_pct_top) |
      (plate_x_in >= shadow_farleft & plate_x_in < shadow_left & 
         plate_z_pct <= shadow_pct_fartop & plate_z_pct >= shadow_pct_farbot) |
      (plate_x_in <= shadow_farright & plate_x_in >= shadow_farleft &
      plate_z_pct >= shadow_pct_farbot & plate_z_pct < shadow_pct_bot)~ 'Shadow',
    plate_x_in < waste_left | plate_x_in > waste_right |
      plate_z_pct > waste_top | plate_z_pct < waste_bot ~ 'Waste',
    .default = 'Chase'
  )) %>% 
  relocate(attack_zone, .after = 'plate_z_pct')

#removing final nas
# NA Values
# release_spin_rate = 1
# spin_axis = 1
#arm_angle = 45
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  drop_na(release_spin_rate, spin_axis, arm_angle)

# Percentage of Data Remaining  
cat('% of Data Remaining: ', round((nrow(statcast_21_24_ordered)/original_num_rows)*100,1), '%', sep = '')


#### Delta Run Change Difference ####
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(run_exp_change = ifelse(swing == 1, post_count_delta_run_exp - new_count_delta_run_exp, post_count_delta_run_exp - count_delta_run_exp)) %>% 
  relocate(run_exp_change, .after = 'post_count_delta_run_exp')

# Getting League Average xwoba
xwobacon <- statcast_21_24_ordered %>% 
  filter(woba_denom == 1, description == 'hit_into_play') %>% 
  group_by(game_year) %>% 
  reframe(league_average_xwobacon = mean(xwOBA, na.rm = TRUE))

# joining it back to statcast dataframe
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  left_join(xwobacon, by = 'game_year')

#### Note: All Code from here on out will have needed to have metrics from models built ####

# adding manual imputations from formula
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(run_exp_change = case_when(
    #2 Strike Cases
    two_strike == 1 & attack_zone == 'Shadow' & swing == 1 & three_balls == 0 ~ 0,
    two_strike == 1 & attack_zone == 'Shadow' & swing == 0 & three_balls == 0 ~ run_exp_change*1.2,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & three_balls == 0 ~ run_exp_change*1.5,
    two_strike == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == "FF" & three_balls == 0 ~ run_exp_change*2,
    # 3 Ball Cases
    three_balls == 1 & attack_zone == 'Shadow' & swing == 1  & two_strike == 0 ~ run_exp_change * 1.2,
    three_balls == 1 & attack_zone == 'Shadow' & swing == 0 & two_strike == 0 ~ run_exp_change * 0.75,
    three_balls == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == 'FF' & two_strike == 0 ~ run_exp_change*1.2,
    .default = run_exp_change
  ))

# adding formulas 
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(contact_metric = ifelse(swing == 1,
                                 run_exp_change * (1- swing_probability) * (1 - contact_probability), run_exp_change * swing_probability * contact_probability),
         xdamage_metric = ifelse(swing == 1, xdamage- league_average_xwobacon, league_average_xwobacon - xdamage))


# adding formulas for xdamage metric
statcast_21_24_ordered <- statcast_21_24_ordered %>% 
  mutate(xdamage_metric_weighted = case_when(
           #2 Strike Cases
           two_strike == 1 & attack_zone == 'Shadow' & swing == 1 & three_balls == 0 ~ 0,
           two_strike == 1 & attack_zone == 'Shadow' & swing == 0  & three_balls == 0~ xdamage_metric*1.2,
           two_strike == 1 & attack_zone == 'Heart' & swing == 0 & three_balls == 0 ~ xdamage_metric*1.5,
           two_strike == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == "FF" & three_balls == 0 ~ xdamage_metric*2,
           # 3 Ball Cases
           three_balls == 1 & attack_zone == 'Shadow' & swing == 1 & two_strike == 0 ~ xdamage_metric * 1.2,
           three_balls == 1 & attack_zone == 'Shadow' & swing == 0  & two_strike == 0~ xdamage_metric * 0.75,
           three_balls == 1 & attack_zone == 'Heart' & swing == 0 & pitch_type == 'FF' & two_strike == 0 ~ xdamage_metric*1.2,
           .default = xdamage_metric
         ))


# getting season stats
season_metrics <- statcast_21_24_ordered %>% 
  group_by(batter, player_name, game_year) %>% 
  reframe(contact_metric = mean(contact_metric, na.rm = TRUE),
          xdamage_metric = mean(xdamage_metric_weighted, na.rm = TRUE), 
          pa = sum(woba_denom == 1, na.rm = TRUE)) %>% 
  filter(pa > 150)

#scaling metrics
season_metrics <- season_metrics %>% 
  group_by(game_year) %>% 
  mutate(contact_metric_scaled = scale(contact_metric)[,1],
         xdamage_metric_scaled = scale(xdamage_metric)[,1]) %>% 
  ungroup()

# converting to scouting scale
season_metrics <- season_metrics %>% 
  mutate(contact_metric_scouting_scale = contact_metric_scaled*10 + 50,
         xdamage_metric_scouting_scale = xdamage_metric_scaled*10 + 50)

# FINAL METRIC
final_metric <- season_metrics %>% 
  mutate(baez_arraez_scale = contact_metric_scouting_scale*0.6 + xdamage_metric_scouting_scale*0.4) %>% 
  dplyr::select(player_name, game_year, pa, contact_metric_scouting_scale, xdamage_metric_scouting_scale, baez_arraez_scale) %>% 
  arrange(desc(baez_arraez_scale))

summary(final_metric$baez_arraez_scale)

write_csv(final_metric, 'baez_arraez_scale.csv')

arraez <- final_metric %>% filter(player_name == 'Arraez, Luis')

write_csv(arraez, 'baez_arraez_scale_arraez.csv')

baez <- final_metric %>% filter(player_name == 'Báez, Javier')

write_csv(baez, 'baez_arraez_scale_baez.csv')


#testing stickiness
lagged_baez_arraez_scale <- final_metric %>% 
  arrange(player_name,game_year)

lagged_baez_arraez_scale <- final_metric %>% 
  group_by(player_name) %>% 
  arrange(game_year) %>% 
  mutate(lagged_baez_arraez_scale = lag(baez_arraez_scale))

lm_model <- lm(baez_arraez_scale ~ lagged_baez_arraez_scale, data = lagged_baez_arraez_scale %>% drop_na(lagged_baez_arraez_scale))

summary_lm <- summary(lm_model) # p = 2e-16

lagged_baez_arraez_scale %>% drop_na(lagged_baez_arraez_scale) %>% 
  mutate(baez_arraez_scale = case_when(
    baez_arraez_scale > 80 ~ 80,
    baez_arraez_scale < 20 ~ 20,
    .default = baez_arraez_scale
  ),
  lagged_baez_arraez_scale = case_when(
    lagged_baez_arraez_scale > 80 ~ 80,
    lagged_baez_arraez_scale < 20 ~ 20,
    .default = lagged_baez_arraez_scale
  )) %>% 
  ggplot(aes(baez_arraez_scale, lagged_baez_arraez_scale, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(20,80),
                     breaks = seq(20,80, by = 10)) +
  scale_y_continuous(limits = c(20,80),
                     breaks = seq(20, 80, by = 10)) +
  annotate('text', x = 70, y = 25, label = paste('r = ', round(corrr::correlate(lagged_baez_arraez_scale$lagged_baez_arraez_scale, lagged_baez_arraez_scale$baez_arraez_scale) %>% 
                                                   pull(x), 3), sep = '')) +
  annotate('text', x = 71.5, y = 23, label = 'p = 2e-16***') +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
  labs(title = 'Baez-Arraez Score Stickiness (Min. 150 PA)',
       size = 'PA',
       y = 'Previous Season Baez-Arraez Score',
       x = 'Current Season Baez-Arraez Score')

# Getting Player wOBA
season_wOBA <- statcast_21_24_ordered %>% 
  group_by(player_name, game_year) %>% 
  reframe(wOBA = sum(woba_value, na.rm =TRUE)/sum(woba_denom, na.rm = TRUE),
          pa = sum(woba_denom, na.rm = TRUE)) %>% 
  filter(pa > 150)

#joining on final metrics
metric_woba_df <- final_metric %>% 
  left_join(season_wOBA, by = c('player_name','game_year', 'pa')) %>% 
  mutate(
    baez_arraez_scale = case_when(
      baez_arraez_scale > 80 ~ 80,
      baez_arraez_scale < 20 ~ 20,
      .default = baez_arraez_scale
    )
  )


woba_lm <- lm(wOBA ~ baez_arraez_scale, data = metric_woba_df)
summary(woba_lm)


metric_woba_df %>% 
  ggplot(aes(baez_arraez_scale, wOBA, size = pa)) +
  geom_point(alpha = 0.3) +
  scale_x_continuous(limits = c(20,80), breaks = seq(20,80, by = 10))+
  geom_abline(slope = coef(woba_lm)[2], intercept = coef(woba_lm)[1]) +
  theme_classic() +
  labs(
    title = 'Yearly wOBA vs Baez-Arraez Scale',
    x = 'Baez-Arraez Scale',
       size = 'PA')

#### Write to Parquet ####
arrow::write_parquet(statcast_21_24_ordered, 'final_statcast_21_24_ordered.parquet')
