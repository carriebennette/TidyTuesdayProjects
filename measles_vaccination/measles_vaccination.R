
library(binom)
library(ggplot2)
library(tidyr)
library(showtext)

# Get the raw data (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-25/readme.md)
measles_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

measles <- measles_raw %>%
  filter(enroll > 0 ) %>%  # a suprising number of schools have zero enrollment
  # get rid of the -1s (these are missing) 
  mutate(mmr = case_when(mmr == -1 ~ NA_real_,
                         TRUE ~ mmr)) %>%
  # Apparently RI, PA, and Arkansas don't have county data; use city instead
  # potentially come back and refine using lat/lon
  mutate(geo_group = case_when(state %in% c("Rhode Island", 
                                            "Arkansas", 
                                            "Pennsylvania") ~ city, 
                               TRUE ~ county)) %>%
  filter(!is.na(geo_group)) %>% # note: FL is dropped (not county or city data..)
  # we could (probably?) assume that MMR rates ~ overall rates (e.g., for states like Iowa that only report overall rates)
  mutate(mmr_adj = case_when(is.na(mmr) & overall != -1 ~ overall,
                             TRUE ~ mmr)) %>% 
  # there are duplicates!
  distinct(name, city, state, county, geo_group, enroll, mmr, mmr_adj, overall, type, xrel, xmed, xper) %>%

  # school with largest enrollment is 2x bigger than second largest (6k vs 3k)
  # school is "West Valley School Prek-6" in Montana...googling suggests enroll is actually closer to n=400
  # https://www.publicschoolreview.com/west-valley-school-profile
  # drop school for now; if this was more than just a fun data viz exercise I'd definitely dig deeper into data collection
  filter(name != "West Valley School Prek-6") %>%
  group_by(state) %>%
  # get state averages (for color scale)
  mutate(state_avg = weighted.mean(mmr, w = enroll, na.rm = T)) %>% 
  ungroup() %>%
  group_by(geo_group, state) %>%
  summarise(mmr_rate = weighted.mean(mmr, w = enroll, na.rm = T),
            enroll = sum(enroll, na.rm = T),
            state_avg = mean(state_avg, na.rm = T)) %>%
  mutate(enroll_group = case_when(enroll < 500 ~ 0.1,
                          enroll >= 500 & enroll < 1000 ~ 0.15,
                          enroll >= 1000 & enroll < 2500 ~ 0.2,
                          enroll >= 2500 & enroll < 5000 ~ 0.25,
                          enroll >= 5000 & enroll < 10000 ~ 0.3,
                          enroll >= 10000 & enroll < 20000 ~ 0.4,
                          enroll >= 20000 & enroll < 50000 ~ 0.5,
                          enroll >= 50000 & enroll < 60000 ~ 0.6,
                          enroll >= 60000 & enroll < 70000 ~ 0.7,
                          enroll >= 70000 & enroll < 80000 ~ 0.8,
                          enroll >= 80000 & enroll < 90000 ~ 0.9,
                          enroll >= 100000 ~ 1,
                          TRUE ~ 1)) %>%
  filter(enroll < 100000 & enroll > 250) %>%
  ungroup()
 

# so I learned today that you can download fancy fonts and use them within R 
# good timing as I no longer have Illustrator until I get a new laptop...

# load the graphics device engine driver that will show the fancy plots
dev.off()
quartz()

fig <- ggplot(measles, aes(enroll, mmr_rate, color = state_avg, alpha = enroll_group)) + 
  geom_point(size = 3.5) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.title.y = element_text(angle = 0, vjust = 1),
        axis.text = element_text(size = 11, family = "roboto"),
        axis.title = element_text(size = 13, family = "roboto"),
        plot.title = element_text(size = 17, face = "bold", family = "roboto"),
        plot.caption = element_text(color = "grey70")) +
  guides(alpha = FALSE) +
  scale_y_continuous("\n", 
                     labels = function(x) paste0(sprintf("%.0f", x),"%")) +
  scale_x_continuous("\nStudent Enrollment\n", 
                     labels = scales::comma) +
  scale_color_gradient(low = "red", 
                       high = "blue", 
                       limits=c(88, 96),
                       oob = scales::squish,
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              title = "State average immunization rate",
                                              barwidth = 15,
                                              barheight = 0.25,
                                              ticks = FALSE,
                                              title.hjust = 0.5)) +
  # annotations! call out Little Rock, AR
  annotate(geom = "curve", x = 28848, y = 68, xend = 20548, yend = 77, 
           curvature = -.23, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 29400, y = 68, size = 3,
           label = "Little Rock, AR had a notably low rate \ngiven it's relatively large enrollment", 
           family = "lato",
           hjust = "left",
           lineheight = 0.9,
           color = "grey50") +
  # call out Brinkley, AR
  annotate(geom = "curve", x = 12000, y = 50, xend = 505, yend = 39.75, 
           curvature = .23, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 12500, y = 50, size = 3,
           label = "When based on a smaller number of students, seemingly large differences are often \ndue to chance. Estimates from less populous counties are shown with lower transparency \nto de-emphasize such variability. Despite having fewer than 500 students, \nBrinkley, AR still manages to stand out for it's exceptionally low rate.", 
           family = "lato",
           hjust = "left",
           lineheight = 0.9,
           color = "grey50") +
  # call out dashed line
  geom_hline(yintercept = 95, linetype = "dashed", size = 0.25, color = "grey40") +
  annotate(geom = "curve", x = 90000, y = 88, xend = 90000, yend = 94, 
           curvature = .23, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 89500, y = 88, size = 3,
           # source: https://www.who.int/immunization/sage/meetings/2017/october/2._target_immunity_levels_FUNK.pdf
           # source: https://illinoisreview.typepad.com/.a/6a00d834515c5469e201b8d0ddb9ce970c-pi
           label = "According to the CDC, 95 percent of a population needs to be \nvaccinated to stop the spread of measles and preserve herd \nimmunity. Many counties appear to fall below this threshold.", 
           hjust = "right",
           lineheight = 0.9,
           family = "lato",
           color = "grey50") +
  # title
  ggtitle("Measles, mumps & rubella immunization rates at schools across 1244 counties/cities in the US") +
  labs(subtitle="According to data collected by The Wall Street Journal",
       caption = "Visualization by @carriebennete \nTwo counties with more than 100k students (Cook County, IL & \nLos Angeles County, CA) are not shown; both had rates above 95%.\nData are aggregated by county (or city when county data is missing)") +
  theme(panel.grid.major = element_line(size = 0.25),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
fig

quartz.save("test", type = "png", device = dev.cur(), dpi = 800)

ggsave("measles_vaccination","MMR_rates.png", dpi = 300)
