library(tidyverse)

### reading in park factors
park_factors <- read_csv('Park_Factors.csv')

park_factors %>% 
  arrange(desc(Park_Factor)) %>% 
  head(5)


### reading in statcast code
statcast_2021 <- read_csv('statcast_2021.csv') %>% 
  select(-'...1')

glimpse(statcast_2021)

statcast_2022 <- read_csv('statcast_2022.csv') %>% 
  mutate(game_date = as.Date(game_date, format = '%Y/%m/%d'))

glimpse(statcast_2022)

statcast_2023 <- read_csv('statcast_2023.csv') %>% 
  mutate(game_date = as.Date(game_date, format = '%Y/%m/%d'))

glimpse(statcast_2023)


statcast_2024 <- read_csv('statcast_2024.csv') %>% 
  mutate(game_date = as.Date(game_date, format = '%Y/%m/%d'))

glimpse(statcast_2024)


statcast_21_24 <- bind_rows(statcast_2021, statcast_2022,
                            statcast_2023, statcast_2024)

remove(statcast_2021, statcast_2022, statcast_2023, statcast_2024) #save space

### write to parquet file
statcast_21_24 <- arrow::write_parquet(statcast_21_24, 'statcast_21_24.parquet')


### read from parquet file
statcast_21_24 <- arrow::read_parquet('statcast_21_24.parquet')