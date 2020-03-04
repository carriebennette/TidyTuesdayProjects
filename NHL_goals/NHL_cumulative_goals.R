library(dplyr)
library(gganimate)
library(ggplot2)
library(lubridate)
library(zoo)

# get the data (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-03/readme.md)
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

#---------- CLEAN RAW DATA ------ #
goals_by_date <- game_goals %>%
  select(player, date, age, team, goals, rank) %>%
  # fix age (prob better way to do this)
  mutate(year = as.numeric(substr(age, 1, 2)),
         days = as.numeric(substr(age, 4, 6))/365.25,
         age = year + days) %>%
  # want the cumulative number of goals by time/age
  group_by(player) %>%
  mutate(cum_goals = order_by(date, cumsum(goals))) %>%
  # now join with players data (for active/retired status) <-- didn't use in the end
  ungroup() %>%
  left_join(top_250 %>% select(player, active, yr_start), by = "player") 

# above creates graph that is not terrible...but there are some annoying seasonal trends (jumps between seasons) that I want to remove
# so I will remove all data points that don't show a 1+ increase in cumulative scoring
# this solution took me a while to figure out (and is likely suboptimally implemented)!

#---------- PREP FOR VISUALIZATION ------ #
goals_cumulative <- goals_by_date %>%
  # going to group across quarter (month wasn't enough given offseason length)
  mutate(yearqtr = as.yearqtr(date)) %>%
  group_by(player, yearqtr) %>%
  mutate(max_cum_goals = max(cum_goals)) %>%
  group_by(player) %>%
  arrange(desc(date)) %>%
  # how much did cumulative goals increase since prior time point?
  mutate(step = max_cum_goals - lead(max_cum_goals)) %>%
  # only keep if there was an increase (to avoid having lots of "steps" in graph)
  # OR observation was last (want to keep long tails)
  filter(step >= 1 | row_number()==n()) %>%
  # need to add in a row so that everyone starts with zero (graph looks funky otherwise)
  group_by(player) %>%
  do(add_row(., .before = 0)) %>%
  # now fill in the missing data for the row just created
  ungroup() %>%
  # need player (but must ungroup first)
  mutate(player = case_when(is.na(player) ~ lead(player, 1), TRUE ~ player)) %>%
  group_by(player) %>% 
  mutate(age = case_when(is.na(age) ~ min(age, na.rm = T) - 0.25, TRUE ~ age),
         rank = case_when(is.na(rank) ~ min(rank, na.rm = T), TRUE ~ rank),
         max_cum_goals = case_when(is.na(max_cum_goals) ~ 0, TRUE ~ max_cum_goals),
         is_700 = case_when(any(max_cum_goals >= 700) ~ TRUE, TRUE ~ FALSE)) %>%
  ungroup() %>%
  # added in another "blank" player so that we can pause animation
  # hacky solution to make gganimate work the way I want...
  do(add_row(., .before = 0)) %>%
  mutate(player = case_when(is.na(player) ~ "blank", TRUE ~ player),
         age = case_when(player == "blank" ~ 20, TRUE ~ age),
         max_cum_goals = case_when(player == "blank" ~ 0, TRUE ~ max_cum_goals)) %>%
  # need to create a 2nd player variable so we can loop through SOME players in animation
  mutate(player_highlight = case_when(player %in% c("Alex Ovechkin", 
                                                    "Wayne Gretzky",
                                                    "Teemu Selanne", 
                                                    "Mark Messier", 
                                                    "Brendan Shanahan",
                                                    "blank") ~ player, 
                                      TRUE ~ NA_character_),
         # make this a factor with set levels so we can control order they will be shown
         player_fct = factor(player_highlight, levels = c("Alex Ovechkin", 
                                                          "Wayne Gretzky",
                                                          "Teemu Selanne",
                                                          "blank",
                                                          "Mark Messier", 
                                                          "Brendan Shanahan"))) %>%
  # and lastly add text (from WaPo) that will accompany animation 
  mutate(text = case_when(player_highlight == "Alex Ovechkin" ~ "Ovechkin is the second-youngest player to score 700 goals.",
                          player_highlight == "Wayne Gretzky" ~ "Gretzky reached this milestone at age 29. While Gretzky's scoring declined \nin the last decade of his career, he had 57 goals in his final 3 seasons.",
                          player_highlight == "Teemu Selanne" ~ "To break Gretzky's record, Ovechkin could look to Teemu Selanne for \ninspiration: Selanne score 232 goals in his final 9 seasons.",
                          player_highlight == "blank" ~ "But since 1980, only three players 35 or older have scored 40+ \ngoals in a season...",
                          player_highlight == "Mark Messier" ~ "Mark Messier did first with 47 goals at age 35 in 1995-96.",
                          player_highlight == "Brendan Shanahan" ~ "And Brendan Shanahan did with 40 goals at age 37 in 2005-06. \nDaniel Alfredsson (not shown) also scored 40 goals at age 35 in 2007-08",
                          TRUE ~ "")) 


#---------- CREATE THE ANIMATED VIZ ------ #
p = ggplot(goals_cumulative, aes(age, max_cum_goals)) +
  geom_hline(yintercept = 700, linetype = "dashed", size = 0.2, color = "grey40") +
  # lots of fiddling
  theme(legend.position = "none", 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.ticks = element_line(color = "grey60", size = 0.2),
        plot.title = element_text(size = 18, 
                                  family="Times", 
                                  face = "bold", 
                                  hjust = 0.5),
        plot.caption = element_text(color = "grey70"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_text(aes(x = 20, label = text), y = 1000, hjust = "left", lineheight = 0.9, color = "grey30", check_overlap = TRUE) +
  ggtitle("Can he keep up the pace?") +
  labs(caption = "Visualization by @carriebennete") +
  geom_line(aes(y = max_cum_goals, group = player_fct, color = factor(player_fct)), size = 1.25) +
  scale_color_manual(values=c("#b00000", "#0d1094", "#de6d16", "#ffffff" , "#4f288f", "#076b01")) +
  # expand() makes graph "fill up" available space (ie no weird gaps) 
  scale_y_continuous("\n", 
                     breaks = c(0, 300, 600, 900), 
                     limits=c(0, 1100), 
                     expand = c(0, 0)) +
  scale_x_continuous("Age", 
                     breaks = c(20, 25, 30, 35, 40, 45),
                     expand = c(0, 0)) +
  # make transition pause on each player
  transition_states(player_fct, 1, 10, wrap = FALSE) +
  # include gray lines for players just shown
  shadow_mark(past = TRUE, exclude_layer = 2, color = "gray70", size = 0.5, alpha = 0.5) +
  exit_recolor(color = "gray80") +
  enter_recolor(color = "gray90")

# control outpt ( so that it's slow enough to read text)
animate(p, nframes = 300, fps = 10, width = 1000, height = 700, res = 150)



