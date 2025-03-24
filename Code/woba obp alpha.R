
#### Loading Packages ####

require(tidyverse)
require(magrittr)
require(ltm)

#### Loading Data ####

SCera <- #read_csv("~/Desktop/everything_else/Coding_Practice/Baseball_R/SCera.csv", show_col_types = FALSE)

#### Data Wrangling ####

Pitchers_temp_2 <- statcast_21_24_ordered %>%
  group_by(pitcher) %>%
  summarise(n_thrown = n())

Batters2 <- statcast_21_24_ordered %>%
  group_by(batter) %>%
  summarise(n_seen = n()) %>%
  left_join(Pitchers_temp, by = c("batter" = "pitcher")) %>%
  mutate(n_thrown = ifelse(is.na(n_thrown), 0, n_thrown)) %>%
  filter(n_seen >= n_thrown) #eliminate players who threw more pitches than they saw

woba_opb_calpha <- statcast_21_24_ordered

woba_opb_calpha$rand <- runif(nrow(woba_opb_calpha)) # assigns a random value between 0-1 to the dataset (used later)

PreppedData_wobaobp <- woba_opb_calpha %>%
  dplyr::select(woba_denom, woba_value, game_year,events, batter, player_name, game_date, at_bat_number, rand) %>% #selecting necessary columns
  filter(
    woba_denom == 1,
    events != "game_advisory",
    events != "ejection",
    batter %in% Batters$batter
  ) %>% #indentifying different batted ball events
  mutate(
    BB = ifelse(events == "walk", 1, 0),
    HBP = ifelse(events == "hit_by_pitch", 1, 0),
    SO = ifelse(events %in% c("strikeout", "strikeout_double_play"), 1, 0),
    H = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0),
    HR = ifelse(events == "home_run", 1 , 0),
    OBP = H + BB + HBP,
    wOBA = woba_value
  ) %>% 
  group_by(batter, game_year) %>% #grouping by batter
  mutate(
    name = first(player_name), #grabbing player name from first row of player name
    PA_car = sum(woba_denom, na.rm = TRUE) # number of plate appearances in the season
  ) %>%
  ungroup() %>%
  filter(PA_car > 450) %>% #filtering for players with 500 or more PA
  group_by(batter, game_year) %>%
  arrange(rand) %>% #Used to randomize sequencing of PA
  mutate(PA_num = row_number()) %>% #getting PA number on the season
  filter(PA_num <= 450) %>% # filtering for when PA are less than 500
  ungroup()

Randomized_Collection_woba_obp <- NULL;for(rerun in 1:10){ # run the for loop once
  for(metric in c("OBP", 'wOBA')){ # for each of these metrics
    for(sampsize in 3:450){ # 3:500 -> number of PA
      
      PA_Matrix_wobaobp <- PreppedData_wobaobp %>%
        pivot_wider(
          id_cols = c(batter, game_year), # make each individual PA it's own column
          names_from = PA_num,
          names_prefix = "PA",
          values_from = metric # given metric from the second nested for loop
        ) %>%
        dplyr::select(-batter)
      
      PA_Matrix_wobaobp <- PA_Matrix_wobaobp[,sample(1:450, sampsize)] #
      
      #print(PA_Matrix)
      
      Randomized_Collection_woba_obp <- Randomized_Collection_woba_obp %>%
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
Randomized_Collection_woba_obp <- Randomized_Collection_woba_obp %>%
  group_by(metric, ss) %>% # grouping by metric and samplesize
  summarise(s_players = mean(s_players), # finding the standard deviation of each player
            alpha = mean(alpha)) #finding the mean alpha for each plate appearance

#Cronbach visual
woba_obp_cronbach <- Randomized_Collection_woba_obp %>%
  ggplot(aes(x = ss, y = alpha, color = metric)) +
  geom_jitter(alpha = 0.5, position = position_jitter(width = 2, height = 0.05, seed = 101)) +
  geom_hline(yintercept = sqrt(0.5),
             linetype = "dashed") +
  ylab("Cronbach's alpha") +
  xlab("Sample Size (PA)") +
  labs(color = "Metric",
       title = 'wOBA & OBP Cronbach Alpha (Yearly 2021-2024)') +
  coord_cartesian(ylim = c(0, 1)) + 
  geom_vline(xintercept = 290, linetype = 'dashed') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.1)) +
  annotate('text', x = 340, y = 1, label = '290 PA') +
  #geom_smooth() +
  theme_classic()

ggsave(filename = 'wOBA vs OBP Cronbach Alpha.png', plot = woba_obp_cronbach, width = 5.75, height = 6.2, units = 'in')
