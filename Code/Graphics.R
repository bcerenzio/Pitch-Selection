#install.packages('colorspace')
library(tidyverse)
library(colorspace)

#### Overall xwOBACON Graph ####
(xwobacon_plot <- statcast_21_24_ordered %>% 
  filter(woba_denom == 1, description == 'hit_into_play') %>% 
  mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
  ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
  stat_summary_2d(bins = 50) +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                    filter(woba_denom == 1, description == 'hit_into_play') %>% 
                                                                    pull(xwOBA), na.rm = TRUE),
                                   limits = c(0.2,0.55),
                                   oob = scales::squish,
                                   name = 'xwOBACON') +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(0,72)) +
  annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
  annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
  annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
  annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
  annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
  annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
  annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
  annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
  annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
  labs(title = 'All Counts') +
  theme(plot.title = element_text(vjust = -15, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('xwobacon_plot.png', plot = xwobacon_plot, bg = 'transparent')



(xwobacon2_plot <- statcast_21_24_ordered %>% 
    filter(woba_denom == 1, description == 'hit_into_play', three_balls == 1, two_strike == 0) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      filter(woba_denom == 1, description == 'hit_into_play') %>% 
                                                                      pull(xwOBA), na.rm = TRUE),
                                     limits = c(0.2,0.55),
                                     oob = scales::squish,
                                     name = 'xwOBACON') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,72)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
    annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = '3 Balls (Excluding 3-2)') +
    theme(plot.title = element_text(vjust = -20, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('threeballxwobacon_plot.png', plot = xwobacon2_plot, bg = 'transparent')



(twostrike_xwobacon_plot <- statcast_21_24_ordered %>% 
    filter(woba_denom == 1, description == 'hit_into_play', two_strike == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      filter(woba_denom == 1, description == 'hit_into_play') %>% 
                                                                      pull(xwOBA), na.rm = TRUE),
                                     limits = c(0.2,0.55),
                                     oob = scales::squish,
                                     name = 'xwOBACON') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,72)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
    annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = '2 Strikes') +
    theme(plot.title = element_text(vjust = -17, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('twostrikexwobacon_plot.png', plot = twostrike_xwobacon_plot, bg = 'transparent')



### Fastballs (RHH POV) ####
# FF
(FF_xwobacon_plot <- statcast_21_24_ordered %>% 
  filter(pitch_type %in% c('FF')) %>% 
  mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
  filter(woba_denom == 1 & description == 'hit_into_play') %>% 
  ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
  stat_summary_2d(bins = 50) +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                    pull(xwOBA), na.rm = TRUE),
                                   limits = c(0.2,0.55),
                                   oob = scales::squish,
                                   name = 'xwOBACON') +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(0,72)) +
  annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
  annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
  annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
  annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  labs(title = 'FF') +
  theme(plot.title = element_text(vjust = -15, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('FF_xwobacon_plot.png', plot = FF_xwobacon_plot, bg = 'transparent')

# SI
(SI_xwobacon_plot <- statcast_21_24_ordered %>% 
  filter(pitch_type %in% c('SI')) %>% 
  mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
  filter(woba_denom == 1 & description == 'hit_into_play') %>% 
  ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
  stat_summary_2d(bins = 50) +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                    pull(xwOBA), na.rm = TRUE),
                                   limits = c(0.2,0.55),
                                   oob = scales::squish,
                                   name = 'xwOBACON') +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(0,72)) +
  annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
  annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
  annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
  annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  labs(title = 'SI') +
  theme(plot.title = element_text(vjust = -19, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('SI_xwobacon_plot.png', plot = SI_xwobacon_plot, bg = 'transparent')

# FC
(FC_xwobacon_plot <- statcast_21_24_ordered %>% 
  filter(pitch_type %in% c('FC')) %>% 
  mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
  filter(woba_denom == 1 & description == 'hit_into_play') %>% 
  ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
  stat_summary_2d(bins = 50) +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                    pull(xwOBA), na.rm = TRUE),
                                   limits = c(0.2,0.55),
                                   oob = scales::squish,
                                   name = 'xwOBACON') +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(0,72)) +
  annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
  annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
  annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
  annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
  annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
  labs(title = 'FC') +
  theme(plot.title = element_text(vjust = -17, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('FC_xwobacon_plot.png', plot = FC_xwobacon_plot, bg = 'transparent')

### Breaking Balls (RHH POV) ####
# CU
(CU_xwobacon_plot <- statcast_21_24_ordered %>% 
   filter(pitch_type %in% c('CU', 'KC')) %>% 
   mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
   filter(woba_denom == 1 & description == 'hit_into_play') %>% 
   ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
   stat_summary_2d(bins = 35) +
   scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                     filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                     pull(xwOBA), na.rm = TRUE),
                                    limits = c(0.2,0.55),
                                    oob = scales::squish,
                                    name = 'xwOBACON') +
   coord_fixed() +
   theme_void() +
   scale_y_continuous(limits = c(0,72)) +
   annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
   annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
   annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
   annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
   annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
   annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
   annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   labs(title = 'CU') +
   theme(plot.title = element_text(vjust = -19, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('CU_xwobacon_plot.png', plot = CU_xwobacon_plot, bg = 'transparent')

# ST
(ST_xwobacon_plot <- statcast_21_24_ordered %>% 
    filter(pitch_type %in% c('ST', 'SV')) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
    ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
    stat_summary_2d(bins = 30) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                      pull(xwOBA), na.rm = TRUE),
                                     limits = c(0.2,0.55),
                                     oob = scales::squish,
                                     name = 'xwOBACON') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,72)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'ST') +
    theme(plot.title = element_text(vjust = -25, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('ST_xwobacon_plot.png', plot = ST_xwobacon_plot, bg = 'transparent')


# SL
(SL_xwobacon_plot <- statcast_21_24_ordered %>% 
    filter(pitch_type %in% c('SL')) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
    ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
    stat_summary_2d(bins = 30) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                      pull(xwOBA), na.rm = TRUE),
                                     limits = c(0.2,0.55),
                                     oob = scales::squish,
                                     name = 'xwOBACON') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,72)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'SL') +
    theme(plot.title = element_text(vjust = -25, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('SL_xwobacon_plot.png', plot = SL_xwobacon_plot, bg = 'transparent')

### Offspeed (RHH POV)####
# CH
(CH_xwobacon_plot <- statcast_21_24_ordered %>% 
   filter(pitch_type %in% c('CH')) %>% 
   mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
   filter(woba_denom == 1 & description == 'hit_into_play') %>% 
   ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
   stat_summary_2d(bins = 30) +
   scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                     filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                     pull(xwOBA), na.rm = TRUE),
                                    limits = c(0.2,0.55),
                                    oob = scales::squish,
                                    name = 'xwOBACON') +
   coord_fixed() +
   theme_void() +
   scale_y_continuous(limits = c(0,72)) +
   annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
   annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
   annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
   annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
   annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
   annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
   annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   labs(title = 'CH') +
   theme(plot.title = element_text(vjust = -25, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('CH_xwobacon_plot.png', plot = CH_xwobacon_plot, bg = 'transparent')


# FS
(FS_xwobacon_plot <- statcast_21_24_ordered %>% 
    filter(pitch_type %in% c('FS')) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>% 
    filter(woba_denom == 1 & description == 'hit_into_play') %>% 
    ggplot(aes(plate_x_in, plate_z_in, z = xwOBA)) +
    stat_summary_2d(bins = 30) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      filter(woba_denom == 1 & description == 'hit_into_play') %>% 
                                                                      pull(xwOBA), na.rm = TRUE),
                                     limits = c(0.2,0.55),
                                     oob = scales::squish,
                                     name = 'xwOBACON') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,72)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'FS') +
    theme(plot.title = element_text(vjust = -25, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('FS_xwobacon_plot.png', plot = FS_xwobacon_plot, bg = 'transparent')

#### Overall Swing% Graph ####
(swingpct_plot <- statcast_21_24_ordered %>% 
   mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
   ggplot(aes(plate_x_in, plate_z_in, z = swing)) +
   stat_summary_2d(bins = 50) +
   scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                     pull(swing), na.rm = TRUE),
                                    limits = c(0.25,0.75),
                                    oob = scales::squish,
                                    name = 'Swing%') +
   coord_fixed() +
   theme_void() +
   scale_y_continuous(limits = c(0,60)) +
   scale_x_continuous(limits = c(-20,20)) +
   annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
   annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
   annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
   annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
   annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
   annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
   annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
   annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
   annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
   annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
   labs(title = 'All Counts') +
   theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('swingpct_plot.png', plot = swingpct_plot, bg = 'transparent')


#2 strikes
(twostrike_swingpct_plot <- statcast_21_24_ordered %>% 
    filter(two_strike == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = swing)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      pull(swing), na.rm = TRUE),
                                    limits = c(0.25,0.75),
                                     oob = scales::squish,
                                     name = 'Swing%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
    annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = '2 Strikes') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('twostrike_swingpct_plot.png', plot = twostrike_swingpct_plot, bg = 'transparent')

#3 Ball
(threeball_swingpct_plot <- statcast_21_24_ordered %>% 
    filter(three_balls == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = swing)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      pull(swing), na.rm = TRUE),
                                     limits = c(0.25,0.75),
                                     oob = scales::squish,
                                     name = 'Swing%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'Overall (3 Ball)') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('threeball_swingpct_plot.png', plot = threeball_swingpct_plot, bg = 'transparent')


#3 ball (Excluding 3-2)
(threeball_swingpct_plot2 <- statcast_21_24_ordered %>% 
    filter(three_balls == 1, two_strike == 0) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = swing)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      pull(swing), na.rm = TRUE),
                                     limits = c(0.25,0.75),
                                     oob = scales::squish,
                                     name = 'Swing%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
    annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = '3 Balls (Excluding 3-2)') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('threeball_swingpct_plot2.png', plot = threeball_swingpct_plot2, bg = 'transparent')


#3-2
(threetwo_swingpct_plot <- statcast_21_24_ordered %>% 
    filter(three_balls == 1, two_strike == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = swing)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>% 
                                                                      pull(swing), na.rm = TRUE),
                                     limits = c(0.25,0.75),
                                     oob = scales::squish,
                                     name = 'Swing%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'Overall (3-2)') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('threetwo_swingpct_plot.png', plot = threetwo_swingpct_plot, bg = 'transparent')



#### Contact Pct (RHH POV) ####
(contactpct_plot <- statcast_21_24_ordered %>% 
    filter(swing == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = contact)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>%
                                                                      filter(swing == 1) %>% 
                                                                      pull(contact), na.rm = TRUE),
                                     limits = c(0.55,0.95),
                                     oob = scales::squish,
                                     name = 'Contact%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
   annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
   annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
   annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
   annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = 'All Counts') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('contactpct_plot.png', plot = contactpct_plot, bg = 'transparent')


(twostrike_contactpct_plot <- statcast_21_24_ordered %>% 
    filter(swing == 1, two_strike == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = contact)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>%
                                                                      filter(swing == 1) %>% 
                                                                      pull(contact), na.rm = TRUE),
                                     limits = c(0.55,0.95),
                                     oob = scales::squish,
                                     name = 'Contact%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1, linetype = 'dashed') +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 14, color = 'gold2', linewidth = 1) +
    annotate('line', x = c(-13.3,13.3), y = 46, color = 'gold2', linewidth = 1) +
    annotate('line', x = -13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    annotate('line', x = 13.3, y = c(14, 46), color = 'gold2', linewidth = 1) +
    labs(title = '2 Strikes') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('twostrike_contactpct_plot.png', plot = twostrike_contactpct_plot, bg = 'transparent')


(threeball_contactpct_plot <- statcast_21_24_ordered %>% 
    filter(swing == 1, two_strike == 0, three_balls == 1) %>% 
    mutate(ifelse(stand == 'L', -plate_x_in, plate_x_in)) %>%
    ggplot(aes(plate_x_in, plate_z_in, z = contact)) +
    stat_summary_2d(bins = 50) +
    scale_fill_continuous_divergingx('RdBu', rev = TRUE, mid = mean(statcast_21_24_ordered %>%
                                                                      filter(swing == 1) %>% 
                                                                      pull(contact), na.rm = TRUE),
                                     limits = c(0.55,0.95),
                                     oob = scales::squish,
                                     name = 'Contact%') +
    coord_fixed() +
    theme_void() +
    scale_y_continuous(limits = c(0,60)) +
    scale_x_continuous(limits = c(-20,20)) +
    annotate('line', x = c(-10,10), y = 18, color = 'black', linewidth = 1) +
    annotate('line', x = c(-10,10), y = 42, color = 'black', linewidth = 1) +
    annotate('line', x = -10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = 10, y = c(18, 42), color = 'black', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 22, color = 'red', linewidth = 1) +
    annotate('line', x = c(-6.67,6.67), y = 38, color = 'red', linewidth = 1) +
    annotate('line', x = -6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    annotate('line', x = 6.67, y = c(22, 38), color = 'red', linewidth = 1) +
    labs(title = 'Overall (3 Ball Excluding 3-2)') +
    theme(plot.title = element_text(vjust = -5, hjust = 0.5, face = 'bold.italic', size = 20)))

ggsave('threeball_strike_contactpct_plot.png', plot = threeball_contactpct_plot, bg = 'transparent')
