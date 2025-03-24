#### Loading Packages ####

require(tidyverse)
require(magrittr)
require(ltm)
### loading in data

SC_baez_arraez <- statcast_21_24_ordered

#### Data Wrangling ####

Pitchers_temp <- SC_baez_arraez %>%
  group_by(pitcher) %>%
  summarise(n_thrown = n())

Batters <- SC_baez_arraez %>%
  group_by(batter) %>%
  summarise(n_seen = n()) %>%
  left_join(Pitchers_temp, by = c("batter" = "pitcher")) %>%
  mutate(n_thrown = ifelse(is.na(n_thrown), 0, n_thrown)) %>%
  filter(n_seen >= n_thrown) #eliminate players who threw more pitches than they saw

SC_baez_arraez$rand <- runif(nrow(SC_baez_arraez)) # assigns a random value between 0-1 to the dataset (used later)

PreppedData_yearly <- SC_baez_arraez %>%
  dplyr::select(batter, player_name, game_date, at_bat_number, rand, xdamage_metric_weighted, contact_metric, zswing, chase, game_year) %>% #selecting necessary columns
  filter(
    batter %in% Batters$batter
  ) %>% #indentifying different batted ball events
  mutate(
    baez_arraez_scale = scale(contact_metric)[,1]*0.6 + scale(xdamage_metric_weighted)[,1]*0.4,
    baez_arraez_scale = baez_arraez_scale*10 + 50
  ) %>% 
  group_by(batter, game_year) %>% #grouping by batter
  mutate(
    name = first(player_name), #grabbing player name from first row of player name
    pitches = n() # number of pitches seen in a season
  ) %>%
  ungroup() %>%
  filter(pitches >= 2000) %>% #filtering for players with 500 or more PA
  group_by(batter, game_year) %>%
  arrange(rand) %>% #Used to randomize sequencing of PA
  mutate(pitch_num = row_number()) %>% #getting pitch number on the season
  filter(pitch_num <= 2000) %>% # filtering for when pitches are less than
  ungroup()


PreppedData_zswing <- SCera %>%
  dplyr::select(batter, player_name, game_date, at_bat_number, rand, xdamage_metric_weighted, contact_metric, zswing, chase) %>% #selecting necessary columns
  filter(
    batter %in% Batters$batter
  ) %>% #indentifying different batted ball events 
  drop_na(zswing) %>% 
  mutate(
    baez_arraez_scale = scale(contact_metric)[,1]*0.6 + scale(xdamage_metric_weighted)[,1]*0.4,
    baez_arraez_scale = baez_arraez_scale*10 + 50
  ) %>% 
  group_by(batter) %>% #grouping by batter
  mutate(
    name = first(player_name), #grabbing player name from first row of player name
    pitches = n() # number of pitches seen in a season
  ) %>%
  ungroup() %>%
  filter(pitches >= 1400) %>% #filtering for players with 500 or more PA
  group_by(batter) %>%
  arrange(rand) %>% #Used to randomize sequencing of PA
  mutate(pitch_num = row_number()) %>% #getting pitch number on the season
  filter(pitch_num <= 1400) %>% # filtering for when pitches are less than
  ungroup()


PreppedData_chase <- SCera %>%
  dplyr::select(batter, player_name, game_date, at_bat_number, rand, xdamage_metric_weighted, contact_metric, zswing, chase) %>% #selecting necessary columns
  filter(
    batter %in% Batters$batter
  ) %>% #indentifying different batted ball events 
  drop_na(chase) %>% 
  mutate(
    baez_arraez_scale = scale(contact_metric)[,1]*0.6 + scale(xdamage_metric_weighted)[,1]*0.4,
    baez_arraez_scale = baez_arraez_scale*10 + 50
  ) %>% 
  group_by(batter) %>% #grouping by batter
  mutate(
    name = first(player_name), #grabbing player name from first row of player name
    pitches = n() # number of pitches seen in a season
  ) %>%
  ungroup() %>%
  filter(pitches >= 1400) %>% #filtering for players with 500 or more PA
  group_by(batter) %>%
  arrange(rand) %>% #Used to randomize sequencing of PA
  mutate(pitch_num = row_number()) %>% #getting pitch number on the season
  filter(pitch_num <= 1400) %>% # filtering for when pitches are less than
  ungroup()

Randomized_Collection_yearly <- NULL;for(rerun in 1:3){ # run the for loop once
  for(metric in c("baez_arraez_scale")){ # for each of these metrics
    for(sampsize in 100:2000){ # 3:2000 -> number of PA
      PA_Matrix_yearly <- PreppedData_yearly %>%
        pivot_wider(
          id_cols = c(batter,game_year), # make each individual PA it's own column
          names_from = pitch_num,
          names_prefix = "Pitch",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      PA_Matrix_yearly <- PA_Matrix_yearly[,sample(1:2000, sampsize)] # sampling for number of pitches
      
      
      
      Randomized_Collection_yearly <- Randomized_Collection_yearly %>%
        rbind(
          data.frame(
            metric = metric,
            ss = sampsize,
            s_players = sd(rowSums(PA_Matrix_yearly)/ncol(PA_Matrix_yearly)), # finding the standard deviation among players at each PA
            alpha = cronbach.alpha(data = PA_Matrix_yearly[,1:sampsize])$alpha # finding the cronboch alpha
          )
        )
    }
    print(paste(rerun, metric)) # prints progress
  }
}

for(rerun in 1:3){ # run the for loop once
  for(metric in c("zswing")){ # for each of these metrics
    for(sampsize in 100:1400){ # 3:2000 -> number of PA
      PA_Matrix <- PreppedData_zswing %>%
        pivot_wider(
          id_cols = batter, # make each individual PA it's own column
          names_from = pitch_num,
          names_prefix = "Pitch",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      PA_Matrix <- PA_Matrix[,sample(1:1400, sampsize)] # sampling for number of pitches
      
      
      
      Randomized_Collection <- Randomized_Collection %>%
        rbind(
          data.frame(
            metric = metric,
            ss = sampsize,
            s_players = sd(rowSums(PA_Matrix)/ncol(PA_Matrix)), # finding the standard deviation among players at each PA
            alpha = cronbach.alpha(data = PA_Matrix[,1:sampsize])$alpha # finding the cronboch alpha
          )
        )
    }
    print(paste(rerun, metric)) # prints progress
  }
}

for(rerun in 1:3){ # run the for loop once
  for(metric in c("chase")){ # for each of these metrics
    for(sampsize in 100:1400){ # 3:2000 -> number of PA
      PA_Matrix <- PreppedData_chase %>%
        pivot_wider(
          id_cols = batter, # make each individual PA it's own column
          names_from = pitch_num,
          names_prefix = "Pitch",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      PA_Matrix <- PA_Matrix[,sample(1:1400, sampsize)] # sampling for number of pitches
      
      
      
      Randomized_Collection <- Randomized_Collection %>%
        rbind(
          data.frame(
            metric = metric,
            ss = sampsize,
            s_players = sd(rowSums(PA_Matrix)/ncol(PA_Matrix)), # finding the standard deviation among players at each PA
            alpha = cronbach.alpha(data = PA_Matrix[,1:sampsize])$alpha # finding the cronboch alpha
          )
        )
    }
    print(paste(rerun, metric)) # prints progress
  }
}

#Averaging out re-runs
Randomized_Collection_yearly <- Randomized_Collection_yearly %>%
  group_by(metric, ss) %>% # grouping by metric and samplesize
  summarise(s_players = mean(s_players), # finding the standard deviation of each player
            alpha = mean(alpha)) #finding the mean alpha for each plate appearance

#Cronbach visual
# Note: Made Minor Changes to the Methodology Last Minute
# and didin't have time to change the cronbach graphic
# so it might look a little different than the slides
Randomized_Collection_yearly %>%
  filter(metric == 'baez_arraez_scale') %>% 
  mutate(metric = 'Baez-Arraez Scale') %>% 
  ggplot(aes(x = ss, y = alpha, color = metric)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = sqrt(0.5),
             linetype = "dashed") +
  ggtitle('Baez-Arraez Cronbach Alpha (2021-2024 Yearly Data)') +
  ylab("Cronbach's alpha") +
  xlab("Sample Size (Pitches)") +
  labs(color = "Metric") +
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.1)) +
  geom_vline(xintercept = 1200, linetype = 'dashed') +
  geom_smooth() +
  annotate('text', x = 1500, y = 1, label = '1200 Pitches\n(Roughly 300 PA)') +
  theme_classic()
