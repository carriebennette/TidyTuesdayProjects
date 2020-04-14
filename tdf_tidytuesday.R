
library(tidyverse)
library(tdf) 
library(ggmap)
library(ggalt)
library(maps)
library(geosphere)
library(lubridate)
library(plyr)

# Get the Data
### hmm, the location data that I wanted (per looking at data dictionary online and then sketching ideas) is not on github...
### internet to the rescue: https://www.kaggle.com/jaminliu/tour-de-france-historical-tour-2018/version/4#unique_stage_geocode.csv
stages <- read_csv("stages_TDF.csv", encoding = "UTF-8")

########################
### GEOLOCATE CITIES ###
########################

# get unique list of locations
unique_locations <- stages %>%
  select(Origin, Destination) %>%
  pivot_longer(c(Origin, Destination), names_to = "type", values_to = "city") %>%
  distinct(city) %>%
  mutate(lat = NA, lon = NA, address = NA) %>%
  # fix a couple cities that google can't find; if this was bigger I'd set up a prospective way to catch these
  mutate(city = case_when(city == "Pla d'Adet" ~ "Saint-Lary", 
                          city == "Merlin-Plage" ~ "Saint-Hilaire-de-Riez", 
                          city == "Montjuïc circuit" ~ "Barcelona", 
                          TRUE ~ city))

# use google API to get lat/lon for each city
for(i in 1:nrow(unique_locations)) {
  
  # ping google for latitutde and longitude
  result <- geocode(unique_locations$city[i], output = "latlona")
  
  # store results
  unique_locations$lon[i] <- as.numeric(result[1])
  unique_locations$lat[i] <- as.numeric(result[2])
  unique_locations$address[i] <- as.character(result[3])
  
}

# Few mistakes need to be fixed manually
unique_locations$lat[unique_locations$city == "Joinville"] <- 48.444
unique_locations$lon[unique_locations$city == "Joinville"] <- 5.139
unique_locations$address[unique_locations$city == "Joinville"] <- "Joinville, France"

unique_locations$lat[unique_locations$city == "Redon"] <- 47.655901
unique_locations$lon[unique_locations$city == "Redon"] <- -2.080030
unique_locations$address[unique_locations$city == "Redon"] <- "Redon, France"

# save file locally so we don't have to keep pinging Google
  #write_csv(unique_locations, "tdf.csv")
  unique_locations <- read_csv("tdf.csv")

# merge lat/lon data back on stages dataset & remove time trials (usually start/end in same city)
stages_geo <- stages %>% 
  mutate(Origin = case_when(Origin == "Pla d'Adet" ~ "Saint-Lary", 
                          Origin == "Merlin-Plage" | Destination == "Merlin-Plage" ~ "Saint-Hilaire-de-Riez", 
                          Origin == "Montjuïc circuit" | Destination == "Montjuïc circuit" ~ "Barcelona", 
                          TRUE ~ Origin)) %>%
  left_join(unique_locations %>% mutate(Origin = city), by = "Origin") %>%
  mutate(orig_lat = lat, 
         orig_lon = lon) %>%
  select(-lat, -lon) %>%
  left_join(unique_locations %>% mutate(Destination = city), by = "Destination") %>%
  mutate(dest_lat = lat,
         dest_lon = lon) %>%
  mutate(dest_lat = ifelse(Date == "7/2/1975" , 46.69778, dest_lat),
         dest_lon = ifelse(Date == "7/2/1975" , -1.9445290, dest_lon)) %>%
  mutate(dest_lat = ifelse(Date == "6/30/1981" , 43.29510, dest_lat),
         dest_lon = ifelse(Date == "6/30/1981" , -0.370797, dest_lon)) %>%
  # Destination of Felsburg, Germany (weird: it's listed in France on map: https://en.wikipedia.org/wiki/1970_Tour_de_France)
  # must be a change in names; only Felsburg now is deep in Germany (so much so it definitely stood out on viz)
  mutate(dest_lat = ifelse(Date == "7/4/1970", 49.31346, dest_lat),
         dest_lon = ifelse(Date == "7/4/1970", 6.7522865, dest_lon)) %>%
  mutate(dest_lat = ifelse(Date == "7/6/1994", 50.81204, dest_lat),
         dest_lon = ifelse(Date == "7/6/1994", -1.0885444, dest_lon)) %>%
  filter(!is.na(dest_lat) & !(Date == "7/27/1939" & Origin == "Bonneval")) %>% # weird mountain time trial
  group_by(Origin) %>%
  dplyr::mutate(count = n()) %>%
  ungroup() %>%
  mutate(year = year(as.Date(Date, "%m/%d/%Y")),
         date = as.Date(Date, "%m/%d/%Y")) %>%
  arrange(year, date) %>%
  mutate(order = row_number()) %>%
  group_by(year) %>%
  mutate(psuedo_stage = row_number()) # some stages are 1a, 1b etc (back when they did 2x per day)
  

#######################
### CREATE BASE MAP ###
#######################

# use "world" base map from map_data() limited to european countries (will also bound later below)
base_map <- map_data("world", region = c("Portugal", "Spain", "France", "Belgium", "Netherlands", "UK", "Italy", "Denmark", "Poland", "Andorra",
                                         "Czech Republic", "Austria", "Switzerland", "Luxembourg", "Germany", "Ireland", "Northern Ireland"))

# set colors
base_map <- c(geom_polygon(aes(long, lat, group = group), 
                     size = 0.1, 
                     color= "#00001C",  
                     fill = "#252241", alpha = 0.8, data = base_map))


# test out what things look like with static plot...not too shabby!
plot <- ggplot() + 
  theme(panel.background =   
        element_rect(fill='#00001C'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  base_map +
  coord_cartesian(xlim = c(-10, 12), ylim = c(36, 58)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_segment(aes(x = orig_lon, xend = dest_lon, y = orig_lat, yend = dest_lat,  size = count), 
               alpha = 0.1, 
               color = "#FEFEF2",  
               data = stages_geo %>% distinct(orig_lon, orig_lat, dest_lat, dest_lon, count)) +
  geom_segment(aes(x = orig_lon, xend = dest_lon, y = orig_lat, yend = dest_lat), 
               alpha = 0.15,
               size = 0.05, 
               color = "#FBFF29",  
               data = stages_geo) 

###################################
### CREATE GREAT CIRCLES (ARCS) ###
###################################
# this blog was a huge help: https://medium.com/@mueller.johannes.j/use-r-and-gganimate-to-make-an-animated-map-of-european-students-and-their-year-abroad-517ad75dca06

# great circles don't add the nice curved arcs in this case (geographic span is too small)
# went down a deep dive learning about bezier curves as an alternative, but ended up thinking straight lines looked better

# need to fortify the data for use with ggplot2
fortify.SpatialLinesDataFrame <- function(model, data, ...){
  ldply(model@lines, fortify)
}

# calculate routes for each row
routes <- gcIntermediate(stages_geo[,c('orig_lon', 'orig_lat')], 
                         stages_geo[,c('dest_lon', 'dest_lat')], 
                         n = 18, 
                         breakAtDateLine = F, 
                         addStartEnd = T, 
                         sp = TRUE)
# fortify to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

# merge to form great circles
routes_count <- data.frame('id' = 1:nrow(stages_geo), 
                           'Stage' = stages_geo$Stage, 
                           'Year' = stages_geo$year,
                           'stage_order' = stages_geo$psuedo_stage)

greatcircles <- merge(fortifiedroutes, routes_count, all.x=T, by='id')

############################
### CREATE THE ANIMATION ###
############################

# want the routes to be revealed in same order as they were ridden
# each route has 20 points (associated with each great arc)
add_delay <- 0
for(i in 1:max(greatcircles$stage_order)){
  greatcircles$order[greatcircles$stage_order==i] <-
    greatcircles$order[greatcircles$stage_order==i] + add_delay
  add_delay = add_delay + 20
}

# want to show each year on it's own (ish); easiest way to do this seems to be outside of gganimate...
# anyone knows of a better way to implement, please let me know!
for (i in c(1903:1914, 1919:1939, 1947:2017)){
  
single <- greatcircles %>%
  filter(Year == i) 

plot <- ggplot() + 
  theme(panel.background =   
          element_rect(fill='#000021'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  base_map +
  # add a couple of the big cities for reference
  annotate(geom="point", x=2.3522, y=48.8566, size = 0.75,
           color="#00001c") +
  annotate(geom="point", x=-1.5536, y=47.2184, size = 0.75,
           color="#00001c") +
  annotate(geom="point", x=4.8357, y=45.7640, size = 0.75,
           color="#00001c") +
  annotate(geom="point", x=1.4442, y=43.6047, size = 0.75,
           color="#00001c") +
  annotate(geom="text", x=2.3522, y=48.5566, label="Paris", size = 3,
           color="#00001c") +
  annotate(geom="text", x=-0.8536, y=47.2184, label="Nantes",  size = 3,
           color="#00001c") +
  annotate(geom="text", x=4.8357, y=45.4640, label="Lyon", size = 3,
           color="#00001c") +
  annotate(geom="text", x=1.4442, y=43.9047, label="Toulouse", size = 3,
           color="#00001c") +
  coord_cartesian(xlim = c(-10, 12), ylim = c(36, 58)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  #trying to add a "glow" effect; think it's kinda lost
  geom_line(aes(long, lat, group = id), alpha = 0.25, 
            size = 0.75, color = "#FDFFAF", data = single) +
  geom_line(aes(long, lat, group = id), alpha = 0.5, 
            size = 0.5, color = "#FDFFAF", data = single) +
  annotate(geom="text", x=-10, y=58, label = "Around and around (and around and around) they go!", 
           hjust = 0, color="#FDFFAF", alpha = 0.75, size = 12) +
  annotate(geom="text", x=-10, y=57, label = paste0("Visualizing the ", i, " stages of the Tour de France"), 
           hjust = 0, color="#FDFFAF", alpha = 0.75, size = 8) +
  annotate(geom="text", x=5.9, y=35.5, label="Visualization: @carriebennette | Data: alastairrushworth/tdf & kaggle/jaminliu", 
           size = 5, color="#FDFFAF", alpha = 0.5) 

anim <- plot + 
  transition_reveal(single$order) 

  animate(anim, duration = 1, fps = 10, width = 900, height = 900, renderer = gifski_renderer())
  anim_save(paste0("tdf", i, ".gif"))

}


# fun facts:
# 1913: first time the route was anticlockwise
# 1926: first time that the race started outside Paris
# 1960s: routes start to be discontinuous... 

# 1994 needs to have UK??
